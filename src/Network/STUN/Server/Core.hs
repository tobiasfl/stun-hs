{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.STUN.Server.Core
    (run)
    where

import qualified Network.STUN.Server.Socket as S
import Network.STUN.Types (Message)
import qualified Network.STUN.Server.Config as C
import qualified Network.STUN.Server.Env as E
import qualified Network.STUN.Types as T
import qualified Network.STUN.Binary as B
import qualified Network.Socket as NS

import Data.Maybe (mapMaybe)
import Data.Word (Word16)

import Control.Monad (forever, void, when)
import Control.Monad.Reader (ReaderT, liftIO, ask, runReaderT, MonadIO, MonadReader)
import qualified Control.Monad.Logger  as ML
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask, bracket)

newtype AppM a = AppM { runApp :: ReaderT E.Env IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader E.Env, MonadThrow, MonadCatch, MonadMask)

instance ML.MonadLogger AppM where
    monadLoggerLog loc _ level msg = do
        env <- ask
        when (level >= env.logLvl) $ do
            liftIO $ fst env.logger $ \time ->
                ML.toLogStr time <> " " <>
                ML.toLogStr ("[" ++ show level ++ "] ") <>
                ML.toLogStr (ML.loc_module loc) <>
                ML.toLogStr (": " :: String) <>
                ML.toLogStr msg <>
                ML.toLogStr ("\n" :: String)

run :: E.Env -> IO ()
run = runReaderT (runApp stunServer)

stunServer :: AppM ()
stunServer = do
  env <- ask
  let sock = env.udpSocketConfig 
  $(ML.logInfo) "Starting server"
  bracket
    (liftIO sock.open)
    (liftIO . sock.close)
    (\openSocket -> do
      liftIO env.serverConfig.onReady
      void $ forever $ do
        (msgOrErr, clientAddr) <- liftIO $ sock.receiveMsg openSocket
        case msgOrErr of
          Right msg -> do
            $(ML.logDebugSH) ("Handling received message:" ++ show msg)
            let responseMaybe =  processStunMessage (msg, clientAddr)
            liftIO $ maybe (pure ()) (sock.sendMsg openSocket clientAddr) responseMaybe
          Left err -> $(ML.logWarnSH) ("Failed to parse received messsage:" ++ err))

processStunMessage :: (Message, NS.SockAddr) -> Maybe Message
processStunMessage (msg, clientAddr)
  | any isUnknown $ T.attributes msg = Just $ T.mkMessage T.BindingErrorResponse tid [T.ErrorCode T.UnknownAttribute420 "UNKNOWN ATTRIBUTE", T.UnknownAttributes (getUnknownAttributeTypes msg)]
  | otherwise = case (T.msgType msg, T.isClassic msg, T.mkAddress clientAddr) of
                           (T.BindingRequest, False, Just addr) -> Just $ T.mkMessage T.BindingSuccessResponse tid [T.mkXORMappedAddress $ B.xorAddress tid addr]
                           (T.BindingRequest, True, Just addr) -> Just $ T.mkClassicMessage T.BindingSuccessResponse tid [T.mkMappedAddress addr]
                           _ -> Nothing
                        where tid = T.transactionId msg

isUnknown :: T.Attribute -> Bool
isUnknown (T.UnknownComprehensionRequired _) = True
isUnknown _ = False

getUnknownAttributeTypes :: Message -> [Word16]
getUnknownAttributeTypes msg = mapMaybe  getUnknown (T.attributes msg)
    where getUnknown (T.UnknownComprehensionRequired x) = Just x
          getUnknown _ = Nothing
