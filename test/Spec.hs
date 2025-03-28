module Main (main) where

import Test.Hspec
import qualified Network.STUN.BinarySpec
import qualified Network.STUN.Server.CoreSpec

main :: IO ()
main = hspec $ do
  describe "Network.STUN.Binary" Network.STUN.BinarySpec.spec
  describe "Network.STUN.Server.Core" Network.STUN.Server.CoreSpec.spec


