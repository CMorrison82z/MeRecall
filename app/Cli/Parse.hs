module Cli.Parse where

import Cli.Commands (addNewEntry, viewAllTags, viewJournal, deleteEntries, editEntry)
import Cli.Types (JournalViewMethod (..), TagSetStrategy (..), ViewOptions (ViewOptions ))
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
              ( let 
                    tags = Tags <$> many (Tag <$> strArgument (metavar "TAGS..."))
                    vOptions = ViewOptions 
                        -- FIXME: This is a temporary hack substitute for a proper "query language" which is WIP at `MeRecall/QueryLang.hs`
                        <$> flag
                            TSSOr
                            TSSAnd
                            ( long "and"
                                <> short 'n'
                                <> help "Get entries whose Tags contain at least all provided Tags. Switch search strategy to `and` mode." )
                        <*> flag
                            ViewInTerminal
                            ViewInBuffer
                            ( long "editor"
                                <> help "View the entries in a temporary buffer (modifying the buffer has no effect)." )
                        <*> option auto
                            ( long "exclude-after"
                                <> short 'x'
                                <> value maxBound
                                <> help "Exclude the tags that come after the N-th tag in the argument list." )
                        <*> switch
                            ( short 'v'
                                <> long "verbose"
                                <> help "Show all details about the journal entries." )
                in viewJournal <$> tags <*> vOptions
              )
              (progDesc "View journal entries. By default, captures any entries that contains at least one of the provided Tags")
          )
        <> command
          "rm"
          ( info
              (deleteEntries <$> some (argument auto (metavar "INDICES...")))
              (progDesc "Delete journal entries at the given indices (obtain via `view --verbose`)")
          )
        <> command
          "ed"
          ( info
              (editEntry <$> argument auto (metavar "INDEX"))
              (progDesc "Delete journal entries at the given indices (obtain via `view --verbose`)")
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
