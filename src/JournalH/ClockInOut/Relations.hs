module JournalH.ClockInOut.Relations where

import Data.Time (UTCTime)
import JournalH.ClockInOut.Types (CompleteSession (CompleteSession, time_interval), IncompleteSession (IncompleteSession, start_time), WorkLog (CompSess, IncompSess))

getStartTime :: WorkLog -> UTCTime
getStartTime (IncompSess IncompleteSession {start_time}) = start_time
getStartTime (CompSess CompleteSession {time_interval = (start_time, _)}) = start_time

isCompSess :: WorkLog -> Bool
isCompSess (CompSess _) = True
isCompSess _ = False
