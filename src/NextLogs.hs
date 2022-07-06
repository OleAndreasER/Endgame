module NextLogs where

import GetLog
import AdvanceStats
import EndgameLog
import EndgameProgram
import EndgameStats


--Endless list of future logs.
nextLogs :: Log -> Stats -> Program -> [Log]
nextLogs log stats program =
    nextLog : (nextLogs nextLog nextStats program)
    where nextStats = advanceStats stats log
          nextLog = getLog "(Future date)" program nextStats
