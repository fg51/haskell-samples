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
    it "with \".\" return Right JpNil" $ do
      P.parseJqFilter "." `shouldBe` Right P.JqNil

    it "with \".[0]\" return Right JqIndex 0 JpNil" $ do
      P.parseJqFilter ".[0]" `shouldBe` Right (P.JqIndex 0 P.JqNil)

    it "with \".fieldName\" return Right JqField \"fieldName\" JqNil" $ do
      P.parseJqFilter ".fieldName" `shouldBe` Right (P.JqField "fieldName" P.JqNil)

    it "with \".[0].fieldName\" return Right JqIndex 0 (JqField \"fieldName\" JqNil)" $ do
      P.parseJqFilter ".[0].fieldName" `shouldBe` Right (P.JqIndex 0 (P.JqField "fieldName" P.JqNil))
