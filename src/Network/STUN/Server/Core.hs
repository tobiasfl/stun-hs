{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
processStunMessage (msg, clientAddr) = case (T.msgType msg, T.mkAddress clientAddr) of
                           (T.BindingRequest, Just addr) -> Just $ T.mkMessage T.BindingSuccessResponse tid [T.mkXORMappedAddress $ B.xorAddress tid addr]
                           _ -> Nothing
                        where tid = T.transactionId msg
