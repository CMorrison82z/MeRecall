module Cli.Types where

import Data.Time (TimeZone)
import JournalH.Types (JournalEntry, Tags)

data TagSetStrategy = TSSAnd | TSSOr
