module Main (main) where

import qualified Network.STUN as STUN

main :: IO ()
main = do
    logger <- STUN.mkLogger STUN.LogToStdout
    let env = STUN.defaultEnv{STUN.logger=logger}
    STUN.run env
