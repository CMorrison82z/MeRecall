module Cli.Parse where

import Cli.Commands (addNewEntry, createNewJobThing, startJobThing, stopJobThing, viewJobThing, viewJournal)
import Cli.Types (JournalViewMethod (ViewInBuffer, ViewInTerminal), TagSetStrategy (TSSAnd, TSSOr))
import JournalH.Types
import Options.Applicative

cli :: Parser (IO ())
cli =
  subparser
    ( command
        "journal"
        (info journal_cli (progDesc "Stuff for actual journalling"))
        <> command
          "worklog"
          (info work_cli (progDesc "Stuff for recording work time stamps stuff"))
    )

journal_cli :: Parser (IO ())
journal_cli =
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
                        <> help "View the entries in a temporary buffer (modifying the buffer has no effect). This can be useful for taking advantage of editor's search features."
                    )
              )
              (progDesc "View journal entries. By default, captures any entries that contains at least one of the provided Tags")
          )
    )

work_cli :: Parser (IO ())
work_cli =
  subparser
    ( command "new" (info (createNewJobThing <$> strArgument (metavar "JOBNAME")) (progDesc "Create new work thing."))
        <> command
          "start"
          ( info
              (startJobThing <$> strArgument (metavar "JOBNAME"))
              (progDesc "Start a job session thing.")
          )
        <> command
          "stop"
          ( info
              (stopJobThing <$> strArgument (metavar "JOBNAME"))
              (progDesc "Stop a job session thing.")
          )
        <> command
          "view"
          ( info
              ( viewJobThing
                  <$> strArgument (metavar "JOBNAME")
                  <*> optional (strOption (long "from" <> short 'f' <> metavar "SINCE Y/M/D"))
                  <*> optional (strOption (long "to" <> short 't' <> metavar "UNTIL Y/M/D"))
              )
              (progDesc "View job sessions")
          )
    )
