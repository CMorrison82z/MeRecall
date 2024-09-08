module Share where

import System.Directory (XdgDirectory (XdgData), doesFileExist, getXdgDirectory)
import System.FilePath ((</>))

appName :: FilePath
appName = "journalh"

jnlhDataDirectory :: IO FilePath
jnlhDataDirectory = getXdgDirectory XdgData appName

defaultJournalFile :: IO FilePath
defaultJournalFile = (</> "journal.txt") <$> jnlhDataDirectory

defaultClockInOutDir :: IO FilePath
defaultClockInOutDir = (</> "clockinout") <$> jnlhDataDirectory

listToMaybeLast :: [a] -> Maybe a
listToMaybeLast = foldl (flip $ const . Just) Nothing

-- Inclusive
isBetweenInc :: (Ord a, Eq a) => a -> a -> a -> Bool
isBetweenInc x y z = x <= z && z <= y

isBetweenIncM :: (Ord a, Eq a) => Maybe a -> Maybe a -> a -> Bool
isBetweenIncM x y z = maybe True (<= z) x && maybe True (z <=) y
