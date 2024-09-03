module JournalH.Share where

import Data.List (isPrefixOf)
import System.Environment (getEnv)
import System.IO (readFile')

journalEntryDocFormat :: String -> String -> String -> String
journalEntryDocFormat time_s tags_s entry_s = '[' : time_s ++ ']' : ' ' : tags_s ++ '\n' : entry_s

preferredTimeFormatting = "%Y-%m-%d %H:%M:%S"

splitAround :: (Eq a) => [a] -> [a] -> ([a], [a])
splitAround [] _ = ([], [])
splitAround sub xs
  | sub `isPrefixOf` xs = ([], drop (length sub) xs)
  | otherwise =
      let (before, after) = splitAround sub (tail xs)
       in (head xs : before, after)
