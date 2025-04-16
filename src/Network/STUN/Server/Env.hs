module Network.STUN.Server.Env
    (Env
    , defaultEnv
    , serverConfig
    , udpSocketConfig
    , logLvl
    , logger
    , mkLogger
    , LogConfig(..))
    where

import qualified Network.STUN.Server.Config as C
import qualified Network.STUN.Server.Socket as S
import qualified Network.Socket as NS
import qualified System.Log.FastLogger as FL
import qualified Control.Monad.Logger as ML

data LogConfig = LogNone | LogToFile FilePath | LogToStdout

data Env = Env
    { serverConfig :: C.ServerConfig
    , udpSocketConfig :: S.SocketConfig
    , logger :: Logger
    , logLvl :: ML.LogLevel
    }

type Logger = (FL.TimedFastLogger, IO ())

mkLogger :: LogConfig -> IO Logger
mkLogger lc = FL.newTimeCache FL.simpleTimeFormat >>= (`FL.newTimedFastLogger` logType) 
            where maxLogFileSizeBytes = 20000000
                  maxLogFiles = 1
                  logType = case lc of
                    LogNone -> FL.LogNone
                    LogToFile fp -> FL.LogFile (FL.FileLogSpec fp maxLogFileSizeBytes maxLogFiles) FL.defaultBufSize
                    LogToStdout -> FL.LogStdout FL.defaultBufSize

noneLogger :: Logger
noneLogger = (const mempty, mempty)

defaultEnv :: Env
defaultEnv = Env
    C.defaultServerConfig
    (S.mkSocketConfigUDP NS.AF_INET (NS.SockAddrInet S.defaultUDPPort S.loopbackAddr))
    noneLogger
    ML.LevelInfo
