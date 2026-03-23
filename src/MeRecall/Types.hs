module MeRecall.Types where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Maybe (isJust, mapMaybe)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, readPTime)
import MeRecall.Share (journalEntryDocFormat, preferredTimeFormatting)
import Text.ParserCombinators.ReadP

newtype Tag = Tag Text
  deriving (Eq)

instance Show Tag where
  show (Tag s) = '@' : T.unpack s

unTag :: Tag -> Text
unTag (Tag t) = t

readPTag :: ReadP Tag
readPTag = Tag . T.pack <$> (skipSpaces >> char '@' >> munch1 isAlphaNum)

-- instance Read Tag where
--   readsPrec _ = readP_to_S readPTag
--   readList = readP_to_S $ many (readPTag <* skipSpaces)

newtype Tags = Tags [Tag]
  deriving (Eq)

instance Show Tags where
  show (Tags xs) = unwords . fmap show $ xs

unTags :: Tags -> [Text]
unTags (Tags ts) = fmap unTag ts

readPTags :: ReadP Tags
readPTags = Tags <$> sepBy readPTag (char ' ')

-- instance Read Tags where
--   readsPrec _ = readP_to_S readPTags

data JournalEntry = JournalEntry
  { entry_time :: UTCTime,
    tags :: Tags,
    entry :: Text
  }

instance Show JournalEntry where
  show (JournalEntry {entry_time, tags, entry}) = journalEntryDocFormat formattedTime (show tags) $ T.unpack entry
    where
      formattedTime = formatTime defaultTimeLocale preferredTimeFormatting entry_time

newtype JEntriesDoc = JEntriesDoc [JournalEntry]

-- NOTE:
-- This is for writing to the data document file. In order to satisfy the expected laws of `Show` and `Read`
instance Show JEntriesDoc where
  show (JEntriesDoc js) = intercalate "\n" . fmap show $ js

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

toJournals :: [JournalLine] -> ReadP [JournalEntry]
toJournals js = maybe pfail pure . sequence $ go js
  where
    go (DateTagsLine (d, t) : jlines) = let
            jentry = T.pack . intercalate "\n" . mapMaybe mapJournalLineString $ (jentryLines)
            (jentryLines, restJs) =
              span
                isJournalLineString
                (jlines)
        in (Just $ JournalEntry {entry_time = d, tags = t, entry = jentry}):go restJs
    go [] = []
    go _ = [Nothing]

readPJournal :: ReadP [JournalEntry]
readPJournal = skipSpaces >> readPJLines >>= toJournals
