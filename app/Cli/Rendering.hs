module Cli.Rendering where

import Cli.Types
import Data.List (isPrefixOf)
import Data.Time (TimeZone, defaultTimeLocale, formatTime, readPTime, utcToZonedTime)
import JournalH.ClockInOut.Types
import JournalH.Share (preferredTimeFormatting)
import JournalH.Types
import String.ANSI

brighterMagenta = rgb 239 207 255

brighterGreen = rgb 239 255 239

-- TODO:
-- 1) Alternating colors could be cool. Also an interesting problem to solve "how to alternate a function".
--    Maybe zip with a repeated list of the 2 functions `[f1, f2, f1, f2, ...]` ?
-- 2) Only highlight to substring that was matched (from `isPrefixOf`).
renderTags :: Tags -> Tags -> String
renderTags (Tags searchedTags) (Tags ts) = unwords . fmap (uncurry tagThing) $ zip (cycle [yellow, brightRed]) ts
  where
    -- TODO:
    -- Resolve badness of pattern matching out the Tag string, but then wrapping it back into a `Tag` for using `show`.
    tagThing color_f (Tag t)
      | any (\(Tag st) -> st `isPrefixOf` t) searchedTags = bold . color_f . show $ Tag t
      | otherwise = faint . color_f . show $ Tag t

journalEntryFormat :: String -> String -> String -> String
journalEntryFormat time_s tags_s entry_s = time_s ++ ' ' : tags_s ++ '\n' : entry_s ++ "\n"

-- showEntryInTimeZone :: TimeZone -> JournalEntry -> String
-- showEntryInTimeZone tz (JournalEntry {entry_time, tags, entry}) = journalEntryFormat ts (show tags) entry
--   where
--     ts = formatTime defaultTimeLocale preferredTimeFormatting (utcToZonedTime tz entry_time) ++ show tz

renderJournalEntry :: Tags -> TimeZone -> (String -> String) -> JournalEntry -> String
renderJournalEntry searchedTags tz color_f JournalEntry {entry_time, tags, entry} =
  journalEntryFormat (brightBlack $ zonedTimeFormatter tz entry_time) (renderTags searchedTags tags) (color_f entry)

zonedTimeFormatter tz t = formatTime defaultTimeLocale preferredTimeFormatting (utcToZonedTime tz t) ++ ' ' : show tz

renderJournalEntries :: Tags -> TimeZone -> JEntriesDoc -> String
renderJournalEntries searchedTags tz (JEntriesDoc js) = unlines . fmap (uncurry (renderJournalEntry searchedTags tz)) $ zip (cycle [brighterMagenta, brighterGreen]) js

renderWorkLog tz (IncompSess IncompleteSession {start_time, note}) =
  unlines [yellow $ "START : " ++ zonedTimeFormatter tz start_time, note]
renderWorkLog tz (CompSess CompleteSession {time_interval = (start_time, end_time), start_note, end_note}) =
  unlines
    [ green $ "START : " ++ zonedTimeFormatter tz start_time,
      start_note,
      red $ "END : " ++ zonedTimeFormatter tz end_time,
      end_note
    ]

renderWorkLogs :: TimeZone -> WorkLogsDoc -> String
renderWorkLogs tz (WorkLogsDoc ws) = init . unlines . fmap (renderWorkLog tz) $ ws
