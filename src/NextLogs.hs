module NextLogs (nextLogs) where

import CurrentLog
import Advance.Cycles
import Types.Log
import Types.Program
import Types.Stats

--Endless list of future logs.
nextLogs' :: Int -> Stats -> Program -> [Log]
nextLogs' i stats program =
    log : (nextLogs' (i+1) nextStats program)
    where log       = currentLog program stats ((show i)++".") 
          nextStats = advanceCycles log stats

nextLogs :: Stats -> Program -> [Log]
nextLogs = nextLogs' 1

