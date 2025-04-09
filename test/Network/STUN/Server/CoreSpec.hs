{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.STUN.Server.CoreSpec (spec) where

import Test.Hspec
--import Test.Hspec.QuickCheck
--import Test.QuickCheck
import Control.Exception (bracket)
import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as SB

import Network.STUN.BinaryUtils
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
      context "When receiving a message with unknown comprehension required attributes" $ do
        it "Responds with an error response message with the UnknownAttribute420 and UnknownAttributes" $ do
          response <- withUDPSocketDo $ \clientSock -> do
            let msg = stunBindRequest 8 <> BS.pack [0x00, 0x2b, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00]
            NS.bind clientSock $ NS.SockAddrInet NS.defaultPort Sock.loopbackAddr
            SB.sendAllTo clientSock msg serverSockAddr
            Bin.deserializeMessage <$> SB.recv clientSock 1500

          let tid = T.TransactionId $ BS.pack [0x53, 0x4f, 0x70, 0x43, 0x69, 0x69, 0x35, 0x4a, 0x66, 0x63, 0x31, 0x7a]
          let expectedResponsAttr = [
                T.ErrorCode T.UnknownAttribute420 "UNKNOWN ATTRIBUTE",
                T.UnknownAttributes [0x002b]]

          let expectedResponse = T.mkMessage T.BindingErrorResponse tid expectedResponsAttr
          response `shouldBe` Right expectedResponse
