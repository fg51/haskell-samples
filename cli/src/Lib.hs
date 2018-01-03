module Lib (someFunc, hello) where

someFunc :: IO ()
someFunc = putStrLn hello

hello :: String
hello = "hello world"
