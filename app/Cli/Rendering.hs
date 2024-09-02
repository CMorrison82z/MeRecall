module Cli.Rendering where

import Cli.Types (ZonedTimeJournalEntry (ZTJE))
import Data.Time (defaultTimeLocale, formatTime, readPTime, utcToZonedTime)
import Share (preferredTimeFormatting, splitAround)
import String.ANSI
import Types

class JRender a where
  render :: a -> String

-- TODO:
-- Alternating colors could be cool. Also an interesting problem to solve "how to alternate a function".
-- Maybe zip ?
instance JRender Tags where
  render (Tags xs) = unwords . fmap (red . show) $ xs

journalEntryFormat :: String -> String -> String -> String
journalEntryFormat time_s tags_s entry_s = time_s ++ ' ' : tags_s ++ '\n' : entry_s

-- showEntryInTimeZone :: TimeZone -> JournalEntry -> String
-- showEntryInTimeZone tz (JournalEntry {entry_time, tags, entry}) = journalEntryFormat ts (show tags) entry
--   where
--     ts = formatTime defaultTimeLocale preferredTimeFormatting (utcToZonedTime tz entry_time) ++ show tz

instance JRender ZonedTimeJournalEntry where
  render (ZTJE (tz, JournalEntry {entry_time, tags, entry})) = journalEntryFormat (brightBlack formattedTime) (render tags) entry
    where
      formattedTime = formatTime defaultTimeLocale preferredTimeFormatting (utcToZonedTime tz entry_time) ++ show tz

instance JRender JEntriesDoc where
  render (JEntriesDoc js) = unlines . fmap render $ js
