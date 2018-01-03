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
copyFile src dst = copyFileWithConvert src dst id


copyFileWithConvert :: Handle -> Handle -> (String -> String) -> IO ()
copyFileWithConvert src dst convert = loop
  where
    loop = do
      isEof <- hIsEOF src
      if isEof
        then return ()
        else do
          line <- hGetLine src
          hPutStrLn dst (convert line)
          loop


foreachLineAndAppend :: Handle -> Handle -> (String -> IO String) -> IO ()
foreachLineAndAppend src dst ioAction =
  foreachLine src $ \line -> do
    output <- ioAction line
    hPutStrLn dst output


foreachLine :: Handle -> (String -> IO ()) -> IO ()
foreachLine src ioAction = loop
  where
    loop = do
      isEof <- hIsEOF src
      if isEof
        then return ()
        else do
          line <- hGetLine src
          ioAction line
          loop
