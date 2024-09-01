{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Cli.Parse (cli)
import Control.Monad (join, when)
import Data.Maybe (fromJust)
import Data.Time (TimeZone, UTCTime (UTCTime), ZonedTime, defaultTimeLocale, formatTime, getCurrentTime, readPTime, utcToZonedTime)
import Options.Applicative
import Share
import String.ANSI (red)
import System.Directory (XdgDirectory (XdgData), doesFileExist, getXdgDirectory)
import System.Environment (getEnv)
import System.IO (readFile')
import System.IO.Temp (emptySystemTempFile)
import System.Process (callProcess)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadP (readP_to_S)
import Types

main :: IO ()
main = join $ customExecParser p opts
  where
    opts =
      info
        (cli <**> helper)
        ( fullDesc
            <> progDesc "Journalling App"
            <> header "PogJournal - A freaking POGGERS journalling app"
        )
    p = prefs showHelpOnEmpty

-- print $ readP_to_S (readPTime True defaultTimeLocale preferredTimeFormatting :: ReadP UTCTime) "2024-08-30 22:12:33"
