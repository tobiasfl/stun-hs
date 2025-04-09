{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Monad (forever, void)
import Control.Exception (bracket)
import Control.Monad.Reader (ReaderT, liftIO, ask, runReaderT, MonadIO, MonadReader)

newtype AppM a = AppM { runApp :: ReaderT E.Env IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader E.Env)

run :: E.Env -> IO ()
run = runReaderT (runApp stunServer)

stunServer :: AppM ()
stunServer = do
  env <- ask
  let sock = env.udpSocketConfig 
  liftIO $ bracket
    sock.open
    sock.close
    (\openSocket -> do
      env.serverConfig.onReady
      void $ forever $ do
        (msgOrErr, clientAddr) <- sock.receiveMsg openSocket
        msg <- either (fail . show) pure msgOrErr
        let responseMaybe = processStunMessage (msg, clientAddr)
        maybe (pure ()) (sock.sendMsg openSocket clientAddr) responseMaybe)


processStunMessage :: (Message, NS.SockAddr) -> Maybe Message
processStunMessage (msg, clientAddr)
  | any isUnknown $ T.attributes msg = Just $ T.mkMessage T.BindingErrorResponse tid [T.ErrorCode T.UnknownAttribute420 "UNKNOWN ATTRIBUTE", T.UnknownAttributes (getUnknownAttributeTypes msg)]
  | otherwise = case (T.msgType msg, T.mkAddress clientAddr) of
                           (T.BindingRequest, Just addr) -> Just $ T.mkMessage T.BindingSuccessResponse tid [T.mkXORMappedAddress $ B.xorAddress tid addr]
                           _ -> Nothing
                        where tid = T.transactionId msg

isUnknown :: T.Attribute -> Bool
isUnknown (T.UnknownComprehensionRequired _) = True
isUnknown _ = False

getUnknownAttributeTypes :: Message -> [Word16]
getUnknownAttributeTypes msg = mapMaybe  getUnknown (T.attributes msg)
    where getUnknown (T.UnknownComprehensionRequired x) = Just x
          getUnknown _ = Nothing
