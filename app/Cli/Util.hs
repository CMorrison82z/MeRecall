module Cli.Util where

import Control.Monad (when)
import System.Directory (doesFileExist, renameFile)
import System.IO.Temp (emptySystemTempFile, writeSystemTempFile)
import System.IO (hGetBuffering, hSetBuffering)
import System.Environment (getEnv)
import System.Process (callProcess)
import System.IO (readFile')
import Data.Char (isAlphaNum)

readFileOrEmpty fp = do
  b <- doesFileExist fp
  if b then readFile fp else pure ""

inputFromEditor :: IO String
inputFromEditor = do
  tempFile <- emptySystemTempFile "journalh_input"

  editor <- getEnv "EDITOR"

  callProcess editor [tempFile]

  readFile' tempFile

editWithEditor :: String -> IO String
editWithEditor s = do
  tempFile <- writeSystemTempFile "journalh_edit" s

  editor <- getEnv "EDITOR"

  callProcess editor [tempFile]

  readFile' tempFile

-- Write to temp, then swap
safeWriteFile f d = do
    tempFile <- writeSystemTempFile "journalh_tmp_write" d

    renameFile tempFile f

withBuffering hdl md act = do
    prevMode <- hGetBuffering hdl
    hSetBuffering hdl md
    res <- act
    hSetBuffering hdl prevMode
    pure res

(!?) :: [a] -> Int -> Maybe a
(x:xs) !? 0 = Just x 
[] !? _ = Nothing
(x:xs) !? i = xs !? (i - 1)
