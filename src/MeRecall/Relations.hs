module MeRecall.Relations where

import Data.Char (toLower)
import Data.List (intersectBy, isPrefixOf, nub, sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import MeRecall.Types (JEntriesDoc (JEntriesDoc), JournalEntry (JournalEntry, tags), Tag (Tag), Tags (Tags), unTags)

textsToTags :: [Text] -> Tags
textsToTags txts = Tags $ fmap Tag txts

-- NOTE:
-- This checks if all `ts` are in JournalEntry tags, but the JournalEntry could have more than `ts`
hasAllTags :: Tags -> JournalEntry -> Bool
hasAllTags ts (JournalEntry {tags = jts}) = hasAllS (unTags ts) $ unTags jts

hasAnyTags :: Tags -> JournalEntry -> Bool
hasAnyTags ts (JournalEntry {tags = jts}) = hasAnyS (unTags ts) $ unTags jts

getSortedTags :: JEntriesDoc -> Tags
getSortedTags (JEntriesDoc jes) = textsToTags . sortBy (\t1 t2 -> compare (fmap toLower $ T.unpack t1) (fmap toLower $ T.unpack t2)) . nub . foldMap (unTags . tags) $ jes

hasAllS txts1 txts2 = (txts1 ==) . intersectBy (\t1 t2 -> T.isPrefixOf t1 t2) txts1 $ txts2

hasAnyS txts1 txts2 = not . null . intersectBy (\t1 t2 -> T.isPrefixOf t1 t2) txts1 $ txts2
