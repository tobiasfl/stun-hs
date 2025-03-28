module Main (main) where

import qualified Network.STUN as STUN

main :: IO ()
main = STUN.run STUN.defaultEnv
