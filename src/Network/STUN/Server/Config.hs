module Network.STUN.Server.Config
    (ServerConfig
    , defaultServerConfig
    , onReady)
    where

data ServerConfig = ServerConfig
    { onReady :: IO ()}

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig (pure ())

