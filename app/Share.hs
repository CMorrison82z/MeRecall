module Share where

import Data.List (isPrefixOf)
import System.Directory (XdgDirectory (XdgData), doesFileExist, getXdgDirectory)
import System.Environment (getEnv)
import System.IO (readFile')
import System.IO.Temp (emptySystemTempFile)
import System.Process (callProcess)

app_name = "MyJournalApp"

getAppDataDirectory = getXdgDirectory XdgData app_name

journalEntryDocFormat :: String -> String -> String -> String
journalEntryDocFormat time_s tags_s entry_s = '[' : time_s ++ ']' : ' ' : tags_s ++ '\n' : entry_s

preferredTimeFormatting = "%Y-%m-%d %H:%M:%S"

readFileOrEmpty fp = do
  b <- doesFileExist fp
  if b then readFile fp else pure ""

splitAround :: (Eq a) => [a] -> [a] -> ([a], [a])
splitAround [] _ = ([], [])
splitAround sub xs
  | sub `isPrefixOf` xs = ([], drop (length sub) xs)
  | otherwise =
      let (before, after) = splitAround sub (tail xs)
       in (head xs : before, after)

editWithEditor :: IO String
editWithEditor = do
  tempFile <- emptySystemTempFile "tempfile.txt"

  editor <- getEnv "EDITOR"

  callProcess editor [tempFile]

  readFile' tempFile
