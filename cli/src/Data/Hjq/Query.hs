{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq.Query (
) where

import Control.Lens
import Control.Monad

import Data.Monoid
import Data.Hjq.Parser
import Data.Aeson
import Data.Aeson.Lens
import Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H -- package: unordered-containers


applyFilter :: JqFilter -> value -> Either T.Text Value
applyFilter (JqField fieldName n) obj@(Object _)
  = join $ noteNotFoundError fieldName (fmap (applyFilter n) (obj ^? key fieldName))
applyFilter (JqIndex index n) array@(Array _)
  = join $ noteOutOfRangeError index (fmap (applyFilter n) (array ^? nth index))
applyFilter JqNil v = Right v
applyFilter f o = Left $ "unexpected pattern : " <> tshow f <> " : " <> tshow o
