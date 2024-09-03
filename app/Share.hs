module Share where

import System.Directory (XdgDirectory (XdgData), doesFileExist, getXdgDirectory)
import System.FilePath ((</>))

appName :: FilePath
appName = "journalh"

jnlhDataDirectory :: IO FilePath
jnlhDataDirectory = getXdgDirectory XdgData appName

defaultJournalFile :: IO FilePath
defaultJournalFile = flip (</>) "journal.txt" <$> jnlhDataDirectory
