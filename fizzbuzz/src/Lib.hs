module Lib (someFunc) where

someFunc :: IO ()
someFunc = do
  let lst = genFizzBuzz
  evalFizzBuzz lst

genFizzBuzz :: FizzBuzz
genFizzBuzz = toFB [1 .. 100]
  where
    toFB :: [Int] -> FizzBuzz
    toFB [] = End
    toFB (n:ns)
      | n `mod` 15 == 0 = Step FizzBuzz (toFB ns)
      | n `mod` 3  == 0 = Step Fizz (toFB ns)
      | n `mod` 5  == 0 = Step Buzz (toFB ns)
      | otherwise = Step (Number n) (toFB ns)

evalFizzBuzz :: FizzBuzz -> IO ()
evalFizzBuzz fb = evalLoop fb evalFizzBuzzAction


type FizzBuzz = Loop FizzBuzzAction

data Loop act = End | Step act (Loop act) deriving Show

data FizzBuzzAction
  = Fizz
  | Buzz
  | FizzBuzz
  | Number Int
  deriving Show

evalLoop :: Loop act -> (act -> IO ()) -> IO ()
evalLoop End _ = return ()
evalLoop (Step action rest) fn = do
  fn action
  evalLoop rest fn

evalFizzBuzzAction :: FizzBuzzAction -> IO ()
evalFizzBuzzAction Fizz = putStrLn "fizz"
evalFizzBuzzAction Buzz = putStrLn "buzz"
evalFizzBuzzAction FizzBuzz = putStrLn "fizzbuzz"
evalFizzBuzzAction (Number n) = print n
