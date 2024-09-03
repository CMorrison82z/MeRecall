{-# LANGUAGE ScopedTypeVariables #-}

module Cli.Commands where

import Cli.Rendering
import Cli.Types (TagSetStrategy (TSSAnd, TSSOr))
import Control.Monad (when)
import Data.Maybe (fromJust)
import Data.Time (TimeZone, UTCTime (UTCTime), ZonedTime, defaultTimeLocale, formatTime, getCurrentTime, getCurrentTimeZone, readPTime, utcToZonedTime)
import Relations (filterAndTags, filterOrTags)
import Share
import Types

addNewEntry :: IO ()
addNewEntry = do
  je <- fromJust <$> newEntry
  app_dir <- getAppDataDirectory
  appDataContents <- readFileOrEmpty app_dir
  let updatedContents = appDataContents ++ '\n' : show je
  -- NOTE:
  -- This is deliberately written like this to force evaluation.
  -- Even `seq` did not solve the problem.
  when (length updatedContents > 0) $
    writeFile app_dir updatedContents

viewJournal :: Tags -> TagSetStrategy -> IO ()
viewJournal ts tsstrat = do
  ad <- getAppDataDirectory
  x <- readFile ad
  tz <- getCurrentTimeZone
  jes <- stratFilter tsstrat ts <$> readIO x

  putStr $ '\n' : renderJournalEntries ts tz (JEntriesDoc jes)
  where
    stratFilter TSSOr = filterOrTags
    stratFilter TSSAnd = filterAndTags

newEntry :: IO (Maybe JournalEntry)
newEntry = do
  contents <- editWithEditor
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
