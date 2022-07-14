module NextLogs where

import GetLog
import AdvanceStats
import Types.Log
import Types.Program
import Types.Stats

--Endless list of future logs.
nextLogs :: Stats -> Program -> Int -> [Log]
nextLogs stats program i =
    log : (nextLogs nextStats program (i+1))
    where log       = getLog ((show i)++".") program stats
          nextStats = advanceStats stats log
