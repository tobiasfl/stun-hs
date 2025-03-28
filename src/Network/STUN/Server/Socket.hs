module Network.STUN.Server.Socket
    ( SocketConfig
    , mkSocketConfigUDP
    , receiveMsg
    , sendMsg
    , close
    , open
    , SocketHandle
    , defaultUDPPort
    , loopbackAddr
    )
    where

import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import qualified Network.STUN.Types as Types
import qualified Network.STUN.Binary as Bin

data SocketConfig = SocketConfig
    { open :: IO SocketHandle
    , receiveMsg :: SocketHandle -> IO (Either String Types.Message, S.SockAddr)
    , sendMsg :: SocketHandle -> S.SockAddr -> Types.Message -> IO ()
    , close :: SocketHandle -> IO ()
    }

newtype SocketHandle = SocketHandle S.Socket

mkSocketConfigUDP :: S.Family -> S.SockAddr -> SocketConfig
mkSocketConfigUDP fam addr = do
  SocketConfig open' receiveMsg' sendMsg' close'
        where open' = do
                sock <- S.socket fam S.Datagram S.defaultProtocol
                S.bind sock addr
                pure $ SocketHandle sock
              receiveMsg' (SocketHandle sock) = do
                (bs, clientAddr) <- SB.recvFrom sock 1500
                let msgOrErr = Bin.deserializeMessage bs
                pure (msgOrErr, clientAddr)
              sendMsg' (SocketHandle sock) toAddr msg = SB.sendAllTo sock (Bin.serializeMessage msg) toAddr
              close' (SocketHandle sock) = S.close sock

defaultUDPPort :: S.PortNumber
defaultUDPPort = 3478

loopbackAddr :: S.HostAddress
loopbackAddr = 0x0100007f
