module Cli.Commands where

import Cli.Rendering
import Cli.Types (JournalViewMethod (ViewInBuffer, ViewInTerminal), TagSetStrategy (TSSAnd, TSSOr))
import Control.Exception (throw)
import Control.Monad (void, when)
import Data.Fixed (Fixed (MkFixed), showFixed)
import Data.Maybe (fromJust)
import Data.Time (TimeZone, UTCTime (UTCTime, utctDay, utctDayTime), ZonedTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime, getCurrentTimeZone, nominalDiffTimeToSeconds, readPTime, secondsToDiffTime, utcToZonedTime)
import Data.Time.Calendar.OrdinalDate
import Debug.Trace (traceM)
import JournalH.ClockInOut.Relations (getStartTime)
import JournalH.ClockInOut.Types
import JournalH.Relations (filterAndTags, filterOrTags)
import JournalH.Types
import Share
import String.ANSI (black, bold, brightGreenBg, greenBg, rgb, white)
import System.Directory (createDirectory, createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (readFile')
import System.IO.Temp (emptySystemTempFile, writeSystemTempFile)
import System.Process (callProcess)

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

addNewEntry :: IO ()
addNewEntry = do
  je <- fromJust <$> newEntry
  appFile <- defaultJournalFile
  appDataContents <- readFileOrEmpty appFile
  let updatedContents = appDataContents ++ '\n' : show je
  -- NOTE:
  -- This is deliberately written like this to force evaluation.
  -- Even `seq` did not solve the problem.
  when (length updatedContents > 0) $
    writeFile appFile updatedContents

viewJournal :: Tags -> TagSetStrategy -> JournalViewMethod -> IO ()
viewJournal ts tsstrat jview = do
  -- TODO:
  -- Warn user if app has never been used before.
  ad <- defaultJournalFile
  x <- readFile ad
  tz <- getCurrentTimeZone
  jes <- stratFilter tsstrat ts <$> readIO x

  case jview of
    ViewInTerminal -> putStr $ '\n' : renderJournalEntries ts tz (JEntriesDoc jes)
    ViewInBuffer -> void . editWithEditor . show $ JEntriesDoc jes
  where
    stratFilter TSSOr = filterOrTags
    stratFilter TSSAnd = filterAndTags

newEntry :: IO (Maybe JournalEntry)
newEntry = do
  contents <- inputFromEditor
  case contents of
    "" -> pure Nothing
    _ -> do
      putStrLn "Provide a list of tags (Space separated)"
      -- TODO:
      -- Check that tag string is alphanumeric. (This is required by the parser)
      l <- Tags . fmap Tag . words <$> getLine
      t <- getCurrentTime

      pure
        . Just
        $ JournalEntry
          { entry_time = t,
            tags = l,
            -- contents always includes a new line character at the end, so I drop it.
            entry = init contents
          }

createNewJobThing :: String -> IO ()
createNewJobThing jobname = do
  dir <- defaultClockInOutDir
  createDirectoryIfMissing True dir
  writeFile (dir </> jobname) ""

startJobThing jobname = do
  jobWorkClockFile <- (</> jobname) <$> defaultClockInOutDir
  WorkLogsDoc works <- tempF =<< readFile jobWorkClockFile
  case listToMaybeLast works of
    Just (IncompSess _) -> error "Cannot start a new session because a previous session is incomplete. Stop it first." -- TODO: Maybe `throw` ?
    _ -> do
      t <- getCurrentTime
      contents <- init <$> inputFromEditor
      -- when (length works > 0) $
      writeFile jobWorkClockFile . show . WorkLogsDoc $ works ++ [IncompSess IncompleteSession {start_time = t, note = contents}]
  where
    tempF "" = pure . WorkLogsDoc $ []
    tempF s = readIO s

stopJobThing jobname = do
  jobWorkClockFile <- (</> jobname) <$> defaultClockInOutDir
  WorkLogsDoc works <- readIO =<< readFile jobWorkClockFile
  case listToMaybeLast works of
    Just (IncompSess IncompleteSession {note, start_time}) -> do
      t <- getCurrentTime
      contents <- init <$> inputFromEditor
      -- when (length works > 0) $
      writeFile jobWorkClockFile . show . WorkLogsDoc $ init works ++ [CompSess CompleteSession {end_note = contents, start_note = note, time_interval = (start_time, t)}]
    _ -> error "No session in progress to stop."

viewJobThing :: String -> Maybe String -> Maybe String -> IO ()
viewJobThing jobname sinceD untilD = do
  (sinceDate :: Maybe Day) <- readIO `mapM` sinceD
  (untilDate :: Maybe Day) <- readIO `mapM` untilD
  tz <- getCurrentTimeZone
  let sinceUTC = (\x -> UTCTime {utctDayTime = secondsToDiffTime 0, utctDay = x}) <$> sinceDate
  let untilUTC = (\x -> UTCTime {utctDayTime = secondsToDiffTime (60 * 60 * 24), utctDay = x}) <$> untilDate
  WorkLogsDoc works <- readIO =<< readFile . (</> jobname) =<< defaultClockInOutDir
  let subWorks =
        let worksFilterPredicate = isBetweenIncM sinceUTC untilUTC . getStartTime
         in filter worksFilterPredicate works
  let time_diff =
        sum
          . fmap
            ( \case
                CompSess CompleteSession {time_interval = (start_time, end_time)} -> end_time `diffUTCTime` start_time
                _ -> 0
            )
          $ subWorks
  putStrLn $ renderWorkLogs tz (WorkLogsDoc subWorks) ++ '\n' : (bold . brightGreenBg . rgb 0 0 0 $ " Net time : " ++ timeDiffFormatter time_diff ++ " ")
  where
    timeDiffFormatter = formatTime defaultTimeLocale "%H:%M:%S"
