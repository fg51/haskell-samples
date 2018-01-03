module Data.Hjq.Parser where
-- module Data.Hjq.Parser (JqFilter(..), parseJqFilter) where

import Data.Text

data JqFilter
  = JqField Text JqFilter
  | JqIndex Int JqFilter
  | JqNil
  deriving (Eq, Show, Read)

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter otherwise = Right JqNil
