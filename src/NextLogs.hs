module NextLogs where

import CurrentLog
import AdvanceCycles
import Types.Log
import Types.Program
import Types.Stats

--Endless list of future logs.
nextLogs :: Stats -> Program -> Int -> [Log]
nextLogs stats program i =
    log : (nextLogs nextStats program (i+1))
    where log       = currentLog program stats ((show i)++".") 
          nextStats = advanceCycles log stats
