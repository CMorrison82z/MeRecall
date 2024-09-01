module Types where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, readPTime)
import Debug.Trace (traceShow, traceShowId, traceShowM)
import Share (AnsiShow, ansiShow, journalEntryFormat, preferredTimeFormatting, splitAround)
import String.ANSI (black, green, red)
import Text.ParserCombinators.ReadP

newtype Tag = Tag String

instance Show Tag where
  show (Tag s) = '@' : s

readPTag :: ReadP Tag
readPTag = Tag <$> (skipSpaces >> char '@' >> munch1 isAlphaNum)

-- instance Read Tag where
--   readsPrec _ = readP_to_S readPTag
--   readList = readP_to_S $ many (readPTag <* skipSpaces)

instance AnsiShow Tag where
  ansiShow (Tag s) = red $ '@' : s

newtype Tags = Tags [Tag]

instance Show Tags where
  show (Tags xs) = unwords . fmap show $ xs

tagsReadP :: ReadP Tags
tagsReadP = fmap Tags $ sepBy readPTag $ char ' '

-- instance Read Tags where
--   readsPrec _ = readP_to_S tagsReadP

instance AnsiShow Tags where
  ansiShow (Tags xs) = unwords . fmap ansiShow $ xs

data JournalEntry = JournalEntry
  { entry_time :: UTCTime,
    tags :: Tags,
    entry :: String
  }

instance Show JournalEntry where
  show (JournalEntry {entry_time, tags, entry}) = journalEntryFormat formattedTime (show tags) entry
    where
      formattedTime = formatTime defaultTimeLocale preferredTimeFormatting entry_time

instance AnsiShow JournalEntry where
  ansiShow (JournalEntry {entry_time, tags, entry}) = journalEntryFormat (black formattedTime) (ansiShow tags) $ green entry
    where
      formattedTime = formatTime defaultTimeLocale preferredTimeFormatting entry_time

-- NOTE:
-- This is for writing to the data document file. In order to satisfy the expected laws of `Show` and `Read`
newtype JEntriesDoc = JEntriesDoc [JournalEntry]

instance Show JEntriesDoc where
  show (JEntriesDoc js) = unlines . fmap show $ js

instance AnsiShow JEntriesDoc where
  ansiShow (JEntriesDoc js) = unlines . fmap ansiShow $ js

data JournalLine = DateTagsLine (UTCTime, Tags) | StringLine String
  deriving (Show)

isJournalLineString :: JournalLine -> Bool
isJournalLineString (DateTagsLine _) = False
isJournalLineString (StringLine _) = True

mapJournalLineString :: JournalLine -> Maybe String
mapJournalLineString (DateTagsLine _) = Nothing
mapJournalLineString (StringLine s) = Just s

readPDateTags :: ReadP (UTCTime, Tags)
readPDateTags = do
  d <- readPDate
  t <- readPTags
  return (d, t)
  where
    readPDate = between (char '[') (char ']') $ readPTime True defaultTimeLocale preferredTimeFormatting
    readPTags = skipSpaces >> tagsReadP

readPJLine :: ReadP JournalLine
readPJLine = (DateTagsLine <$> readPDateTags) <++ (StringLine <$> munch (/= '\n'))

readPJLines :: ReadP [JournalLine]
readPJLines = endBy readPJLine $ char '\n'

toJournals :: [JournalLine] -> [JournalEntry]
toJournals js = go js []
  where
    go (DateTagsLine (d, t) : jlines) acc = go restJs (acc ++ [JournalEntry {entry_time = d, tags = t, entry = jentry}])
      where
        -- NOTE:
        -- `init` is to get rid of the trailing newline character introduced by `unlines`
        jentry = init . unlines . mapMaybe mapJournalLineString $ jentryLines
        (jentryLines, restJs) =
          span
            isJournalLineString
            jlines
    go [] acc = acc
    go _ _ = undefined

readPJournal :: ReadP [JournalEntry]
readPJournal = toJournals <$> readPJLines

-- OLD Stuff.
-- May want to use it again, but using parsing style from above
-- readPJournalEntry :: ReadP JournalEntry
-- readPJournalEntry = do
--   jeTime <- between (char '[') (char ']') $ readPTime True defaultTimeLocale preferredTimeFormatting
--   jeTags <- between skipSpaces (char '\n') tagsReadP
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
