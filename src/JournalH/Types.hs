module JournalH.Types where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, readPTime)
import Debug.Trace (trace, traceShow, traceShowId, traceShowM)
import JournalH.Share (journalEntryDocFormat, preferredTimeFormatting)
import Text.ParserCombinators.ReadP

newtype Tag = Tag String
  deriving (Eq)

instance Show Tag where
  show (Tag s) = '@' : s

readPTag :: ReadP Tag
readPTag = Tag <$> (skipSpaces >> char '@' >> munch1 isAlphaNum)

-- instance Read Tag where
--   readsPrec _ = readP_to_S readPTag
--   readList = readP_to_S $ many (readPTag <* skipSpaces)

newtype Tags = Tags [Tag]
  deriving (Eq)

instance Show Tags where
  show (Tags xs) = unwords . fmap show $ xs

readPTags :: ReadP Tags
readPTags = fmap Tags $ sepBy readPTag (char ' ') -- <* eof

-- instance Read Tags where
--   readsPrec _ = readP_to_S readPTags

data JournalEntry = JournalEntry
  { entry_time :: UTCTime,
    tags :: Tags,
    entry :: String
  }

instance Show JournalEntry where
  show (JournalEntry {entry_time, tags, entry}) = journalEntryDocFormat formattedTime (show tags) entry
    where
      formattedTime = formatTime defaultTimeLocale preferredTimeFormatting entry_time

-- NOTE:
-- This is for writing to the data document file. In order to satisfy the expected laws of `Show` and `Read`
newtype JEntriesDoc = JEntriesDoc [JournalEntry]

instance Show JEntriesDoc where
  show (JEntriesDoc js) = unlines . fmap show $ js

instance Read JEntriesDoc where
  readsPrec _ = readP_to_S (JEntriesDoc <$> readPJournal <* eof)

data JournalLine = DateTagsLine (UTCTime, Tags) | StringLine String
  deriving (Show)

isJournalLineString :: JournalLine -> Bool
isJournalLineString = isJust . mapJournalLineString

mapJournalLineString :: JournalLine -> Maybe String
mapJournalLineString (DateTagsLine _) = Nothing
mapJournalLineString (StringLine s) = Just s

readPDateTags :: ReadP (UTCTime, Tags)
readPDateTags = do
  d <- readPDate
  t <- readPSTags
  return (d, t)
  where
    readPDate = between (char '[') (char ']') $ readPTime True defaultTimeLocale preferredTimeFormatting
    readPSTags = skipSpaces >> readPTags

readPJLine :: ReadP JournalLine
readPJLine = (DateTagsLine <$> readPDateTags) <++ (StringLine <$> munch (/= '\n'))

-- TODO:
-- Would be nice to remove the occasional trailing line due to `sepBy`. using `endBy` can cause problems if the last line doesn't have a newline character.
readPJLines :: ReadP [JournalLine]
readPJLines = sepBy readPJLine $ char '\n'

toJournals :: [JournalLine] -> [JournalEntry]
toJournals js = go (js) []
  where
    go (DateTagsLine (d, t) : jlines) acc = go restJs (acc ++ [JournalEntry {entry_time = d, tags = t, entry = jentry}])
      where
        -- NOTE:
        -- `init` is to get rid of the trailing newline character introduced by `unlines`
        jentry = init . unlines . mapMaybe mapJournalLineString $ (jentryLines)
        (jentryLines, restJs) =
          span
            isJournalLineString
            (jlines)
    go [] acc = acc
    go _ _ = undefined

readPJournal :: ReadP [JournalEntry]
readPJournal = toJournals <$> (skipSpaces >> readPJLines)

-- OLD Stuff.
-- May want to use it again, but using parsing style from above
-- readPJournalEntry :: ReadP JournalEntry
-- readPJournalEntry = do
--   jeTime <- between (char '[') (char ']') $ readPTime True defaultTimeLocale preferredTimeFormatting
--   jeTags <- between skipSpaces (char '\n') readPTags
--   entryStr <- manyTill get (optional (char '\n') >> checkEndOfEntry)
--   return $ JournalEntry jeTime jeTags entryStr
--
-- checkEndOfEntry :: ReadP ()
-- checkEndOfEntry = do
--   remainingInput <- look
--   case remainingInput of
--     s | endOfEntryIdentifier `isPrefixOf` s -> return ()
--     _ -> pfail
--
-- -- instance Read JournalEntry where
-- --   readsPrec _ = readP_to_S readPJournalEntry
--
--
-- -- instance Read JEntriesDoc where
-- --   readsPrec _ = readP_to_S (JEntriesDoc <$> endBy readPJournalEntry (string endOfEntryIdentifier >> char '\n'))
--
--
