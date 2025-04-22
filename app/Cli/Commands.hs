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
import System.Console.Wizard (line, nonEmpty, retry, run, validator, retryMsg)
import System.Console.Wizard.BasicIO (basicIO)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.IO (readFile')
import Text.PrettyPrint.Boxes (render)

addNewEntry :: IO ()
addNewEntry = do
  dir <- appDataDirectory 
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
            user_tags <-
              Tags
                . fmap Tag
                . words
                . fromJust
                <$> ( run
                        . basicIO
                        . retryMsg "One or more invalid tags. Must be AlphaNumeric"
                        . validator (all (\c -> isAlphaNum c || isSpace c))
                        . nonEmpty
                        . line
                        -- FIXME: This Prompt string isn't being output. WHAT THE **** !?
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
      putStrLn rendered_journal_entries
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
