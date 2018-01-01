module Lib (someFunc) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_)
import System.IO
  ( stdout, Handle, FilePath, IOMode(..)
  , openFile, hClose, hIsEOF, hGetLine, hPutStrLn)
import System.Environment (getArgs)
import Data.Char (toUpper)

someFunc :: IO ()
someFunc = do
  filePaths <- getArgs
  concatMultiFiles filePaths stdout

concatMultiFiles :: [FilePath] -> Handle -> IO ()
concatMultiFiles filePaths dst =
  forM_ filePaths $ \filePath ->
    bracket
      (openFile filePath ReadMode)
      (\hdl -> hClose hdl)
      (\hdl -> copyFile hdl dst)


copyFile :: Handle -> Handle -> IO ()
copyFile src dst = loop
  where
    loop = do
      isEof <- hIsEOF src
      if isEof
        then return ()
        else do
          line <- hGetLine src
          hPutStrLn dst line
          loop
