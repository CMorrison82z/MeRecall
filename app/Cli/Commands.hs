module Cli.Commands where

import Cli.Rendering
import Cli.Types (JournalViewMethod (ViewInBuffer, ViewInTerminal), TagSetStrategy (TSSAnd, TSSOr), Verbosity (..))
import Cli.Util
import Control.Monad (void)
import Data.Bool (bool)
import Data.Char (isAlphaNum, isSpace)
import Data.Maybe (fromJust, fromMaybe)
import Data.Time (getCurrentTime, getCurrentTimeZone)
import MeRecall.Relations (filterAndTags, filterOrTags, getSortedTags)
import MeRecall.Types
import Share
import System.Console.Terminal.Size (Window (..), size)
import System.Console.Wizard (line, nonEmpty, retry, run, validator)
import System.Console.Wizard.BasicIO (basicIO)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.IO (readFile')
import Text.PrettyPrint.Boxes (render)

addNewEntry :: IO ()
addNewEntry = do
  dir <- jnlhDataDirectory
  createDirectoryIfMissing True dir

  je <-
    newEntry
      >>= ( \case
              Just x -> pure x
              Nothing -> ioError . userError $ "Nothing entered"
          )

  appFile <- defaultJournalFile
  appendFile appFile $ '\n' : show je
  where
    newEntry :: IO (Maybe JournalEntry)
    newEntry =
      ( \case
          "" -> pure Nothing
          contents -> do
            -- TODO:
            -- Check that tag string is alphanumeric. (This is required by the parser)
            user_tags <-
              Tags
                . fmap Tag
                . words
                . fromJust
                <$> ( run
                        . basicIO
                        . retry
                        . validator (all (\c -> isAlphaNum c || isSpace c))
                        . nonEmpty
                        . line
                        $ "Provide a list of tags (Space separated) :"
                    )

            t <- getCurrentTime

            pure
              . Just
              $ JournalEntry
                { entry_time = t,
                  tags = user_tags,
                  -- contents always includes a new line character at the end, so I drop it.
                  entry = init contents
                }
      )
        =<< inputFromEditor

viewJournal :: Tags -> TagSetStrategy -> JournalViewMethod -> Verbosity -> IO ()
viewJournal ts tsstrat jview verbosity = do
  ad <- defaultJournalFile
  doesFileExist ad >>= bool (ioError . userError $ "There are no journal entries") (pure ())
  x <- readFile' ad
  tz <- getCurrentTimeZone
  jes <- stratFilter tsstrat ts <$> readIO x
  Window {width = window_width} <- fromMaybe (Window {width = 80, height = 24}) <$> size

  case jview of
    ViewInTerminal ->
      putStr rendered_journal_entries
      where
        rendered_journal_entries = case verbosity of
          Normal -> renderJournalEntries window_width (JEntriesDoc jes)
          Verbose -> renderJournalEntriesV ts tz (JEntriesDoc jes)
    ViewInBuffer -> void . editWithEditor . show $ JEntriesDoc jes
  where
    stratFilter TSSOr = filterOrTags
    stratFilter TSSAnd = filterAndTags

viewAllTags :: IO ()
viewAllTags = do
  ad <- defaultJournalFile
  doesFileExist ad >>= bool (ioError . userError $ "There are no journal entries") (pure ())
  x <- readFile ad
  jes <- readIO x

  -- I use an odd number so that the colors are interlaced.
  putStr $ '\n' : (render . makePaddedTable 7 . words . renderTags (Tags []) . getSortedTags $ jes)
