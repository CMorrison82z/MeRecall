module Cli.Commands where

import Cli.Rendering
import Debug.Trace (trace)
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Cli.Types (JournalViewMethod (..), TagSetStrategy (..), ViewOptions (..))
import Cli.Util
import Control.Monad (void, when, filterM)
import Data.Bool (bool)
import Text.Read (readMaybe)
import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import System.IO (stdout, BufferMode(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromJust, fromMaybe)
import Data.Time (getCurrentTime, getCurrentTimeZone)
import MeRecall.Relations (getSortedTags, hasAnyTags, hasAllTags)
import MeRecall.Types
import Share
import System.Console.Terminal.Size (Window (..), size)
import System.Console.Wizard
import System.Console.Wizard.BasicIO
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
                . fmap (Tag . T.pack)
                . words
                . fromJust
                <$> (withBuffering stdout NoBuffering . run
                        . basicIO
                        . retryMsg "One or more invalid tags. Must be AlphaNumeric"
                        . validator (all (\c -> isAlphaNum c || isSpace c))
                        . nonEmpty
                        -- NOTE: `wizard` implementation for `Line` for `BasicIO` is wrong, does not output prompt string. So do it manually...
                        $ outputLn "Provide a list of tags (Space separated) :"
                        >> line ""
                    )

            t <- getCurrentTime

            pure
              . Just
              $ JournalEntry
                { entry_time = t,
                  tags = user_tags,
                  -- contents always includes a new line character at the end, so I drop it.
                  entry = T.pack $ init contents
                }
      )
        =<< inputFromEditor

editEntry :: Int -> IO ()
editEntry index = do
  ad <- defaultJournalFile
  doesFileExist ad >>= bool (ioError . userError $ "There are no journal entries") (pure ())
  x <- readFile' ad
  JEntriesDoc allJEntries <- readIO x

  let (prevEntries, toEditEntry:restEntries) = splitAt index allJEntries

  finishEdittedEntry  <- editEntry toEditEntry >>= ( \case
          Just x -> pure x
          Nothing -> ioError . userError $ "Error. Result is an invalid Journal Entry"
      )
  
  -- https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Exception.html#v:evaluate : 
  let edittedJournal = show . JEntriesDoc $ prevEntries ++ finishEdittedEntry:restEntries

  safeWriteFile ad edittedJournal

  where
    editEntry :: JournalEntry -> IO (Maybe JournalEntry)
    editEntry e =
        ( \case
            "" -> pure Nothing
            contents -> pure $ fmap (\(JEntriesDoc (jentry:_)) -> jentry) $ readMaybe contents
        )
          =<< editWithEditor (show e)

deleteEntries :: [Int] -> IO ()
deleteEntries deleteIndices = do
  ad <- defaultJournalFile
  doesFileExist ad >>= bool (ioError . userError $ "There are no journal entries") (pure ())
  x <- readFile' ad
  JEntriesDoc allJEntries <- readIO x

  let userConfirmDelete ::  Int -> Wizard BasicIO Bool
      userConfirmDelete i = do
        candidateEntry <- liftMaybe $ allJEntries !? i
        outputLn . T.unpack $ entry candidateEntry
        -- `character` considers the `\n` from the previous in the next prompt, causing an immediiate `retry` which duplicates the `output` message.
        -- Therefore, use `fmap head line`
        ynChar <- retry . validator (\c -> c == 'y' || c == 'n') $ output renderDeletePrompt >> fmap head (line "")
        
        pure $ case ynChar of
          'y' -> True
          'n' -> False
          _   -> False -- Should be unreachable, but false just because
      go jes [] _ = jes
      go [] _ _ = []
      go (je:jes) (di:dis) i | i == di = go jes dis (i + 1)
                             | otherwise = je:go jes (di:dis) (i + 1)
  
  confirmedIndices <- withBuffering stdout NoBuffering . fmap (fromMaybe []) . run $ filterM userConfirmDelete deleteIndices

  let edittedJournal = show $ JEntriesDoc $ go allJEntries confirmedIndices 0

  safeWriteFile ad edittedJournal

viewJournal :: Tags -> ViewOptions -> IO ()
viewJournal queriedTags ViewOptions {excludeAfter, tagSetStrategy, viewMethod, verbose} = do
  ad <- defaultJournalFile
  doesFileExist ad >>= bool (ioError . userError $ "There are no journal entries") (pure ())
  x <- readFile' ad
  tz <- getCurrentTimeZone
  JEntriesDoc allJEntries <- readIO x

  let Tags queriedUntags = queriedTags
      filtererF = bool (filter $ stratFilter tagSetStrategy . snd) id $ queriedUntags == []
        -- Zip with indices, used in `verbose` mode
      (jIndices, filteredJEntries) = unzip . filtererF $ zip [0..] allJEntries

  Window {width = window_width} <- fromMaybe (Window {width = 80, height = 24}) <$> size

  case viewMethod of
    ViewInTerminal ->
      putStrLn 
        $ bool 
            (renderJournalEntries window_width $ JEntriesDoc filteredJEntries) 
            (renderJournalEntriesV queriedTags jIndices tz $ JEntriesDoc filteredJEntries) 
            verbose
    ViewInBuffer -> void . editWithEditor . show $ JEntriesDoc filteredJEntries
  where
    excludedTags = let Tags ts' = queriedTags in Tags $ drop (excludeAfter - 1) ts'
    stratFilter TSSOr je = not (hasAnyTags excludedTags je) && hasAnyTags queriedTags je
    stratFilter TSSAnd je = not (hasAnyTags excludedTags je) && hasAllTags queriedTags je

viewAllTags :: IO ()
viewAllTags = do
  ad <- defaultJournalFile
  doesFileExist ad >>= bool (ioError . userError $ "There are no journal entries") (pure ())
  x <- readFile ad
  jes <- readIO x

  -- I use an odd number so that the colors are interlaced.
  putStr $ '\n' : (render . makePaddedTable 7 . words . renderTags (Tags []) . getSortedTags $ jes)
