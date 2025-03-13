module Main (main) where

import Test.Hspec
import qualified Network.STUN.BinarySpec

main :: IO ()
main = hspec $ do
  describe "Network.STUN.Binary" Network.STUN.BinarySpec.spec
