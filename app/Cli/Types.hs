module Cli.Types where

data ViewOptions = ViewOptions {
        tagSetStrategy :: TagSetStrategy,
        viewMethod :: JournalViewMethod,
        excludeAfter :: Int,
        verbose :: Bool
    }

data TagSetStrategy = TSSOr | TSSAnd

data JournalViewMethod = ViewInTerminal | ViewInBuffer
