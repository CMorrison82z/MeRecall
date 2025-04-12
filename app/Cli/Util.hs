module Cli.Util where

import System.Directory (doesFileExist)
import System.IO.Temp (emptySystemTempFile, writeSystemTempFile)
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
