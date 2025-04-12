module Cli.Parse where

import Cli.Commands (addNewEntry, viewAllTags, viewJournal)
import Cli.Types (JournalViewMethod (ViewInBuffer, ViewInTerminal), TagSetStrategy (TSSAnd, TSSOr), Verbosity (..))
import MeRecall.Types
import Options.Applicative
import Share (defaultJournalFile)

cli :: Parser (IO ())
cli =
  subparser
    ( command "new" (info (pure addNewEntry) (progDesc "Add a new entry to the journal"))
        <> command
          "view"
          ( info
              ( (viewJournal . Tags <$> some (Tag <$> strArgument (metavar "TAGS...")))
                  <*> flag
                    TSSOr
                    TSSAnd
                    ( long "and"
                        <> short 'n'
                        <> help "Get entries whose Tags contain at least all provided Tags. Switch search strategy to `and` mode."
                    )
                  <*> flag
                    ViewInTerminal
                    ViewInBuffer
                    ( long "editor"
                        <> help "View the entries in a temporary buffer (modifying the buffer has no effect)."
                    )
                  <*> flag
                    Normal
                    Verbose
                    ( short 'v'
                        <> long "verbose"
                        <> help "View the entries in a temporary buffer (modifying the buffer has no effect)."
                    )
              )
              (progDesc "View journal entries. By default, captures any entries that contains at least one of the provided Tags")
          )
        <> command
          "tags"
          ( info
              (pure viewAllTags)
              (progDesc "View journal entries. By default, captures any entries that contains at least one of the provided Tags")
          )
        <> command
          "path"
          ( info
              (pure $ defaultJournalFile >>= putStrLn)
              (progDesc "Output path to data file.")
          )
    )
