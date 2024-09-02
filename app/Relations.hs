module Relations where

import Data.List (intersect)
import Types (JEntriesDoc (JEntriesDoc), JournalEntry (JournalEntry, tags), Tags (Tags))

-- NOTE:
-- This checks if all `ts` are in JournalEntry tags, but the JournalEntry could have more than `ts`
hasAllTags :: Tags -> JournalEntry -> Bool
hasAllTags (Tags ts) (JournalEntry {tags = (Tags jts)}) = (ts ==) . intersect ts $ jts

hasAnyTags :: Tags -> JournalEntry -> Bool
hasAnyTags (Tags ts) (JournalEntry {tags = (Tags jts)}) = not . null . intersect ts $ jts

filterAndTags :: Tags -> JEntriesDoc -> [JournalEntry]
filterAndTags ts (JEntriesDoc jes) = filter (hasAllTags ts) jes

filterOrTags :: Tags -> JEntriesDoc -> [JournalEntry]
filterOrTags ts (JEntriesDoc jes) = filter (hasAnyTags ts) jes
