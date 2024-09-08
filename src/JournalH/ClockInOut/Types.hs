module JournalH.ClockInOut.Types
  ( readPWorkLog,
    WorkLogsDoc (WorkLogsDoc),
    WorkLog (IncompSess, CompSess),
    IncompleteSession (IncompleteSession, start_time, note),
    CompleteSession (CompleteSession, time_interval, start_note, end_note),
  )
where

import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, readPTime)
import Debug.Trace (trace, traceShow, traceShowId, traceShowM)
import JournalH.Share (journalEntryDocFormat, preferredTimeFormatting)
import Text.ParserCombinators.ReadP

data WorkLog = CompSess CompleteSession | IncompSess IncompleteSession

instance Show WorkLog where
  show (CompSess x) = show x
  show (IncompSess x) = show x

data CompleteSession = CompleteSession
  { time_interval :: (UTCTime, UTCTime),
    start_note :: String,
    end_note :: String
  }

instance Show CompleteSession where
  show CompleteSession {time_interval = (start_time, end_time), start_note, end_note} =
    init . unlines $
      [ "START:" ++ timeFormatter start_time,
        start_note,
        "END:" ++ timeFormatter end_time,
        end_note
      ]
    where
      timeFormatter = formatTime defaultTimeLocale preferredTimeFormatting

data IncompleteSession = IncompleteSession
  { start_time :: UTCTime,
    note :: String
  }

instance Show IncompleteSession where
  show IncompleteSession {start_time, note} =
    init . unlines $ ["START:" ++ formattedTime, note]
    where
      formattedTime = formatTime defaultTimeLocale preferredTimeFormatting start_time

newtype WorkLogsDoc = WorkLogsDoc [WorkLog]

instance Show WorkLogsDoc where
  show (WorkLogsDoc x) = init . unlines . fmap show $ x

instance Read WorkLogsDoc where
  readsPrec _ = readP_to_S (WorkLogsDoc <$> readPWorkLog <* eof)

data ClockInOutLine = CIOT ClockInOutTime | StringLine String
  deriving (Show)

data ClockInOutTime = Start UTCTime | End UTCTime
  deriving (Show)

isClockInOutString :: ClockInOutLine -> Bool
isClockInOutString = isJust . mapClockInOutString

mapClockInOutString :: ClockInOutLine -> Maybe String
mapClockInOutString (CIOT (Start _)) = Nothing
mapClockInOutString (CIOT (End _)) = Nothing
mapClockInOutString (StringLine s) = Just s

readPClockInOut :: ReadP ClockInOutTime
readPClockInOut = (Start <$> (string "START:" >> readPDate)) <++ (End <$> (string "END:" >> readPDate))
  where
    readPDate = skipSpaces >> readPTime True defaultTimeLocale preferredTimeFormatting

readPCLine :: ReadP ClockInOutLine
readPCLine = (CIOT <$> readPClockInOut) <++ (StringLine <$> munch (/= '\n'))

readPCLines :: ReadP [ClockInOutLine]
readPCLines = sepBy readPCLine $ char '\n'

toWorkLogs :: [ClockInOutLine] -> [WorkLog]
toWorkLogs cs = go cs []
  where
    go (CIOT (Start st) : clines) acc = case restCs of
      (CIOT (End et) : restCs') -> go restCs'' $ acc ++ [CompSess $ CompleteSession {time_interval = (st, et), start_note = startNoteEntry, end_note = endNoteEntry}]
        where
          endNoteEntry = init . unlines . mapMaybe mapClockInOutString $ endNoteLines
          (endNoteLines, restCs'') =
            span
              isClockInOutString
              restCs'
      _ -> acc ++ [IncompSess $ IncompleteSession {start_time = st, note = startNoteEntry}]
      where
        -- NOTE:
        -- `init` is to get rid of the trailing newline character introduced by `unlines`
        startNoteEntry = init . unlines . mapMaybe mapClockInOutString $ startNoteLines
        (startNoteLines, restCs) =
          span
            isClockInOutString
            clines
    go [] acc = acc
    go _ _ = undefined

readPWorkLog :: ReadP [WorkLog]
readPWorkLog = toWorkLogs <$> (skipSpaces >> readPCLines)
