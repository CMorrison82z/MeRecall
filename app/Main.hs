module Main where

import Cli.Parse (cli)
import Control.Monad (join, when)
import Options.Applicative
import Share

main :: IO ()
main = join $ customExecParser p opts
  where
    opts =
      info
        (cli <**> helper)
        ( fullDesc
            <> progDesc appName
            <> header
              ( appName
                  ++ " - A simple journaling app"
              )
        )
    p = prefs showHelpOnEmpty
