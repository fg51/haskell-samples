module LibSpec (main, spec) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

import Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hello" $ do
    it "return hello world" $ do
      Lib.hello `shouldBe` "hello world"

  -- describe "decode" $ \xs ->
  --   prop "reverses encoded string" \xs ->
  --     decode (encode xs) == xs
