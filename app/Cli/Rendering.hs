module Cli.Rendering where

import Cli.Types
import Data.Time (TimeZone, defaultTimeLocale, formatTime, readPTime, utcToZonedTime)
import JournalH.Share (preferredTimeFormatting, splitAround)
import JournalH.Types
import String.ANSI

-- TODO:
-- Alternating colors could be cool. Also an interesting problem to solve "how to alternate a function".
-- Maybe zip ?
renderTags :: Tags -> Tags -> String
renderTags (Tags searchedTags) (Tags ts) = unwords . fmap tagThing $ ts
  where
    tagThing t
      | t `elem` searchedTags = bold . yellow . show $ t
      | otherwise = faint . yellow . show $ t

journalEntryFormat :: String -> String -> String -> String
journalEntryFormat time_s tags_s entry_s = time_s ++ ' ' : tags_s ++ '\n' : entry_s ++ "\n"

-- showEntryInTimeZone :: TimeZone -> JournalEntry -> String
-- showEntryInTimeZone tz (JournalEntry {entry_time, tags, entry}) = journalEntryFormat ts (show tags) entry
--   where
--     ts = formatTime defaultTimeLocale preferredTimeFormatting (utcToZonedTime tz entry_time) ++ show tz

renderJournalEntry :: Tags -> TimeZone -> JournalEntry -> String
renderJournalEntry searchedTags tz JournalEntry {entry_time, tags, entry} = journalEntryFormat (brightBlack formattedTime) (renderTags searchedTags tags) (brightMagenta entry)
  where
    formattedTime = formatTime defaultTimeLocale preferredTimeFormatting (utcToZonedTime tz entry_time) ++ ' ' : show tz

renderJournalEntries :: Tags -> TimeZone -> JEntriesDoc -> String
renderJournalEntries searchedTags tz (JEntriesDoc js) = unlines . fmap (renderJournalEntry searchedTags tz) $ js
