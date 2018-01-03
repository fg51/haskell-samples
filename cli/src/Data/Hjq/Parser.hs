{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq.Parser (JqFilter(..), parseJqFilter) where

-- import Control.Applicative ((<*))
import Data.Attoparsec.Text (parse, feed, Result, endOfInput, Parser)
import Data.Text (Text)

data JqFilter
  = JqField Text JqFilter
  | JqIndex Int JqFilter
  | JqNil
  deriving (Eq, Show, Read)

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = showParseResult $ parse (jqFilterParser <* endOfInput) s `feed` ""


jqFilterParser :: Parser JqFilter
jqFilterParser = undefined


showParseResult :: Show a => Result a -> Either Text a
showParseResult = undefined
