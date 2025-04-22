module Cli.Rendering where

import Data.List (isPrefixOf, transpose, intercalate)
import Data.List.Split (chunksOf)
import Data.Time (TimeZone, defaultTimeLocale, formatTime, utcToZonedTime)
import MeRecall.Share (preferredTimeFormatting)
import MeRecall.Types
import String.ANSI
import Text.PrettyPrint.Boxes

type AnsiRenderer = (String -> String)

brighterMagenta, brighterGreen :: AnsiRenderer
brighterMagenta = rgb 239 207 255
brighterGreen = rgb 239 255 239

-- Compute the maximum width for each column
computeColumnWidths :: [[String]] -> [Int]
computeColumnWidths rows = map (maximum . map length) (transpose rows)

-- Pad each string in a row to match the column's width
padColumns :: [Int] -> [String] -> [Box]
padColumns widths row = [text (padRight w item) | (w, item) <- zip widths row]
  where
    padRight n s = s ++ replicate (n - length s) ' '

terminalSeparator window_width = replicate window_width '-'

-- Create a table layout with padding
makePaddedTable :: Int -> [String] -> Box
makePaddedTable cols items =
  let rows = chunksOf cols items -- Split the list into rows
      widths = computeColumnWidths rows -- Find the max width for each column
      paddedRows = map (padColumns widths) rows -- Pad each column in every row
      alignedRows = map (hsep 2 left) paddedRows -- Horizontally combine columns
   in vsep 0 left alignedRows -- Vertically combine rows

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

renderJournalEntry :: (String -> String) -> JournalEntry -> String
renderJournalEntry color_f = color_f . entry

renderJournalEntries :: Int -> JEntriesDoc -> String
renderJournalEntries window_width (JEntriesDoc js) =
  intercalate ('\n' : terminalSeparator window_width ++ "\n")
 . fmap ( uncurry renderJournalEntry) $ zip (cycle [brighterMagenta, brighterGreen]) js

renderJournalEntryV :: Tags -> TimeZone -> (String -> String) -> JournalEntry -> String
renderJournalEntryV searchedTags tz color_f JournalEntry {entry_time, tags, entry} =
  journalEntryFormat (brightBlack $ zonedTimeFormatter tz entry_time) (renderTags searchedTags tags) (color_f entry)
  where
    journalEntryFormat time_s tags_s entry_s = time_s ++ ' ' : tags_s ++ '\n' : entry_s

zonedTimeFormatter tz t = formatTime defaultTimeLocale preferredTimeFormatting (utcToZonedTime tz t) ++ ' ' : show tz

renderJournalEntriesV :: Tags -> TimeZone -> JEntriesDoc -> String
renderJournalEntriesV searchedTags tz (JEntriesDoc js) = unlines . fmap (uncurry (renderJournalEntryV searchedTags tz)) $ zip (cycle [brighterMagenta, brighterGreen]) js
