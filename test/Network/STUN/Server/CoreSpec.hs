{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Network.STUN.Server.CoreSpec (spec) where

import Test.Hspec
--import Test.Hspec.QuickCheck
--import Test.QuickCheck
import Control.Exception (bracket)
import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as SB

import qualified Network.STUN.Server.Config as C
import qualified Network.STUN.Server.Socket as Sock
import qualified Network.STUN.Server.Env as E
import qualified Network.STUN.Server.Core as S
import qualified Network.STUN.Types as T
import qualified Network.Socket as NS
import qualified Network.STUN.Binary as Bin

withserver :: E.Env -> IO () -> IO ()
withserver env action = do
    onReadyCalled <- newEmptyMVar
    bracket
      (forkIO $ S.run env{E.serverConfig=env.serverConfig{C.onReady = putMVar onReadyCalled ()}})
      killThread
      (const (takeMVar onReadyCalled >> action))

serverSockAddr :: NS.SockAddr
serverSockAddr = NS.SockAddrInet Sock.defaultUDPPort Sock.loopbackAddr

withUDPSocketDo :: (NS.Socket -> IO a) -> IO a
withUDPSocketDo = bracket (NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol) NS.close

spec :: Spec
spec = do
  around_ (withserver E.defaultEnv) $ do
    describe "STUN UDP Server end to end" $ do
      it "Responds XORMappedAddress with XOR'ed client port and address to STUN Binding requests" $ do
        let tid = T.TransactionId $ BS.pack $ replicate 12 0x00
        (response, NS.SockAddrInet clientPort clientHostAddr) <- withUDPSocketDo $ \clientSock -> do
          let request = T.mkMessage T.BindingRequest tid []
          NS.bind clientSock $ NS.SockAddrInet NS.defaultPort Sock.loopbackAddr
          SB.sendAllTo clientSock (Bin.serializeMessage request) serverSockAddr
          clientAddr <- NS.getSocketName clientSock

          response <- Bin.deserializeMessage <$> SB.recv clientSock 1500
          pure (response, clientAddr)

        let expectedResponsAttr = T.mkXORMappedAddress $ Bin.xorAddress tid $ T.mkIPv4Address clientPort clientHostAddr
        let expectedResponse = T.mkMessage T.BindingSuccessResponse (T.TransactionId $ BS.pack $ replicate 12 0x00) [expectedResponsAttr]
        response `shouldBe` Right expectedResponse
