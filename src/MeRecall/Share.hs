module MeRecall.Share where

journalEntryDocFormat :: String -> String -> String -> String
journalEntryDocFormat time_s tags_s entry_s = '[' : time_s ++ ']' : ' ' : tags_s ++ '\n' : entry_s

preferredTimeFormatting = "%Y-%m-%d %H:%M:%S"
