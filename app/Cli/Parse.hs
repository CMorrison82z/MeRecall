module Cli.Parse where

import Cli.Commands (addNewEntry, viewJournal)
import Cli.Types (TagSetStrategy (TSSAnd, TSSOr))
import Options.Applicative
import Types

cli :: Parser (IO ())
cli =
  subparser
    ( command "new" (info (pure addNewEntry) (progDesc "Add a new entry to the journal"))
        <> command
          "view"
          ( info
              ( (viewJournal . Tags <$> many (Tag <$> strArgument (metavar "TAGS...")))
                  <*> flag TSSAnd TSSOr (long "or" <> help "Switch search strategy to `or` mode.")
              )
              (progDesc "View journal entries")
          )
    )
