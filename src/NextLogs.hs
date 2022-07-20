module NextLogs (nextLogs, nextLog, nextStats, nextLogAndStats) where

import CurrentLog
import Advance.Cycles
import Advance.PRs
import Types.Log
import Types.Program
import Types.Stats

--Endless list of future logs.
nextLogs' :: Int -> Stats -> Program -> [Log]
nextLogs' i stats program =
    nextLog' : nextLogs' (i+1) nextStats' program
    where
        label = show i ++ "."
        (nextLog', nextStats') = nextLogAndStats stats program label


nextLogs :: Stats -> Program -> [Log]
nextLogs = nextLogs' 1

nextLog :: Stats -> Program -> String -> Log
nextLog stats program label = fst $ nextLogAndStats stats program label

nextStats :: Stats -> Program -> String -> Stats
nextStats stats program label = snd $ nextLogAndStats stats program label

nextLogAndStats :: Stats -> Program -> String -> (Log, Stats)
nextLogAndStats stats program label =
    (nextLog', nextStats')
    where 
        currentLog' :: Stats -> Log
        currentLog' stats' = currentLog program stats' label
        log = currentLog' stats
        advancedCycles = advanceCycles log stats
        nextLog' = currentLog' advancedCycles
        nextStats' = advancePRs nextLog' advancedCycles