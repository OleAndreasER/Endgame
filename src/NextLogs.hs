module NextLogs where

import GetLog
import AdvanceStats
import EndgameLog
import EndgameProgram
import EndgameStats


--Endless list of future logs.
nextLogs :: Stats -> Program -> String -> [Log]
nextLogs stats program label =
    log : (nextLogs nextStats program label)
    where log = getLog label program stats
          nextStats = advanceStats stats log
