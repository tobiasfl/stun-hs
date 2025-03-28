module Network.STUN.Server.Env
    (Env
    , defaultEnv
    , serverConfig
    , udpSocketConfig)
    where

import qualified Network.STUN.Server.Config as C
import qualified Network.STUN.Server.Socket as S
import qualified Network.Socket as NS

data Env = Env
    { serverConfig :: C.ServerConfig
    , udpSocketConfig :: S.SocketConfig
    }

defaultEnv :: Env
defaultEnv = Env C.defaultServerConfig (S.mkSocketConfigUDP NS.AF_INET (NS.SockAddrInet S.defaultUDPPort S.loopbackAddr))
