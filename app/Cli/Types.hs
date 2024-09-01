module Cli.Types where

import Types (Tags)

data Commands = NewEntry | ViewEntries ViewOptions

data TagSetStrategy = TSSAnd | TSSOr

data ViewOptions = ViewOptions
  { tags :: Tags,
    strategy :: TagSetStrategy
  }
