module Cli.Parse where

import Cli.Commands (addNewEntry, viewJournal)
import Cli.Types (TagSetStrategy (TSSAnd, TSSOr))
import JournalH.Types
import Options.Applicative

cli :: Parser (IO ())
cli =
  subparser
    ( command "new" (info (pure addNewEntry) (progDesc "Add a new entry to the journal"))
        <> command
          "view"
          ( info
              ( (viewJournal . Tags <$> many (Tag <$> strArgument (metavar "TAGS...")))
                  <*> flag TSSOr TSSAnd (long "or" <> help "Switch search strategy to `or` mode.")
              )
              (progDesc "View journal entries")
          )
    )
