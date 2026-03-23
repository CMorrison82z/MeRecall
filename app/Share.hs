module Share where

import System.Directory (XdgDirectory (XdgData), getXdgDirectory, listDirectory)
import System.FilePath ((</>))
import Data.Functor ((<&>))

appName :: FilePath
appName = "merecall"

appDescription :: String
appDescription = "A simple journaling app. Tag system makes for effective recall of information."

appDataDirectory :: IO FilePath
appDataDirectory = getXdgDirectory XdgData appName

defaultJournalFile :: IO FilePath
defaultJournalFile =  appDataDirectory <&> (</> "journal_entries")

listToMaybeLast :: [a] -> Maybe a
listToMaybeLast = foldl (flip $ const . Just) Nothing

-- Inclusive
isBetweenInc :: (Ord a, Eq a) => a -> a -> a -> Bool
isBetweenInc lower_b upper_b x = lower_b <= x && x <= upper_b

isBetweenIncM :: (Ord a, Eq a) => Maybe a -> Maybe a -> a -> Bool
isBetweenIncM x y z = maybe True (<= z) x && maybe True (z <=) y
