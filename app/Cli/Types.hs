module Cli.Types where

import Data.Time (TimeZone)
import Types (JournalEntry, Tags)

data TagSetStrategy = TSSAnd | TSSOr
