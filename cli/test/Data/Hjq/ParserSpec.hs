{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq.ParserSpec (main, spec) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
-- import Test.QuickCheck

import qualified Data.Hjq.Parser as P

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseJqFilter" $ do
    it "with \".\" return JpNil" $ do
      P.parseJqFilter "." `shouldBe` Right P.JqNil

  -- describe "decode" $ \xs ->
  --   prop "reverses encoded string" \xs ->
  --     decode (encode xs) == xs
