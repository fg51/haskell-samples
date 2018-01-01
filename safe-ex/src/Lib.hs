module Lib (someFunc) where

import Control.Applicative
import qualified Control.Monad.Trans.Except as EX


someFunc :: IO ()
someFunc = do
  print $ calc 50
  print $ calc 0

safeDiv :: Integer -> Integer -> EX.Except String Integer
safeDiv k n
  | n == 0 = EX.throwE $ "Illegal division by zero. k:" ++ show k
  | otherwise = return (k `div` n)

calc :: Integer -> Either String Integer
calc n = EX.runExcept $ do
  EX.catchE
    (do
      x <- 100 `safeDiv` n
      y <- 100 `safeDiv` (x - 1)
      return y)
    (\_ -> return 0)


anotherSample :: EX.Except [String] Integer
anotherSample = EX.throwE ["Some Error"] <|> EX.throwE ["other Error"] <|> return 123
