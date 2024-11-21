module JournalH.Relations where

import Data.Foldable (Foldable (fold))
import Data.List (intersect, intersectBy, isPrefixOf, nub, sort)
import JournalH.Types (JEntriesDoc (JEntriesDoc), JournalEntry (JournalEntry, tags), Tag (Tag), Tags (Tags))

tagsToStrings :: Tags -> [String]
tagsToStrings (Tags ts) = fmap (\(Tag t) -> t) ts

stringsToTags :: [String] -> Tags
stringsToTags ss = Tags $ fmap Tag ss

-- NOTE:
-- This checks if all `ts` are in JournalEntry tags, but the JournalEntry could have more than `ts`
hasAllTags :: Tags -> JournalEntry -> Bool
hasAllTags (Tags ts) (JournalEntry {tags = (Tags jts)}) = (ts ==) . intersectBy (\(Tag s1) (Tag s2) -> isPrefixOf s1 s2) ts $ jts

hasAnyTags :: Tags -> JournalEntry -> Bool
hasAnyTags (Tags ts) (JournalEntry {tags = (Tags jts)}) = not . null . intersectBy (\(Tag s1) (Tag s2) -> isPrefixOf s1 s2) ts $ jts

filterAndTags :: Tags -> JEntriesDoc -> [JournalEntry]
filterAndTags ts (JEntriesDoc jes) = filter (hasAllTags ts) jes

filterOrTags :: Tags -> JEntriesDoc -> [JournalEntry]
filterOrTags ts (JEntriesDoc jes) = filter (hasAnyTags ts) jes

getSortedTags :: JEntriesDoc -> Tags
getSortedTags (JEntriesDoc jes) = stringsToTags . sort . nub . foldMap (tagsToStrings . tags) $ jes
