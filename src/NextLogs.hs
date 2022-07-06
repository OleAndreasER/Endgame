module NextLogs where

import GetLog
import AdvanceStats
import EndgameLog
import EndgameProgram
import EndgameStats

--Endless list of future logs.
nextLogs :: Stats -> Program -> Int -> [Log]
nextLogs stats program i =
    log : (nextLogs nextStats program (i+1))
    where log       = getLog ((show i)++".") program stats
          nextStats = advanceStats stats log
