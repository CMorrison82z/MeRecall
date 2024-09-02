module Cli.Types where

import Data.Time (TimeZone)
import Types (JournalEntry, Tags)

data Commands = NewEntry | ViewEntries ViewOptions

data TagSetStrategy = TSSAnd | TSSOr

data ViewOptions = ViewOptions
  { tags :: Tags,
    strategy :: TagSetStrategy
  }

newtype ZonedTimeJournalEntry = ZTJE (TimeZone, JournalEntry)
