module NextLogs
    ( nextLogs
    , nextLog
    , nextStats
    , nextLogAndStats
    , firstLogAndStats
    , getNextLogAndStats
    , getNextLogs
    ) where

import FileHandling
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
    (nextLog', nextStats') =
        nextLogAndStats stats program label

nextLogs :: Stats -> Program -> [Log]
nextLogs = nextLogs' 1

nextLog :: Stats -> Program -> String -> Log
nextLog stats program =
    fst . nextLogAndStats stats program

nextStats :: Stats -> Program -> String -> Stats
nextStats stats program =
    snd . nextLogAndStats stats program

nextLogAndStats :: Stats -> Program -> String -> (Log, Stats)
nextLogAndStats stats program label =
    (nextLog', nextStats')
  where
    advancedCycles = advanceCycles program stats
    nextLog' = currentLog program advancedCycles label
    nextStats' = advancePRs nextLog' advancedCycles

--Does not advance stats prior to getting the log.
firstLogAndStats :: Stats -> Program -> String -> (Log, Stats)
firstLogAndStats stats program label =
    (firstLog, nextStats')
  where 
    firstLog   = currentLog program stats label
    nextStats' = advancePRs firstLog stats

getNextLogAndStats :: String -> IO (Log, Stats)
getNextLogAndStats label = do
    stats <- readStats
    program <- readProgram
    logs <- readLogs
    pure $ case logs of
        [] -> firstLogAndStats stats program label
        _  -> nextLogAndStats stats program label

getNextLogs :: IO [Log]
getNextLogs = do
    stats <- readStats
    program <- readProgram
    logs <- readLogs
    let (firstLog, nextStats') = firstLogAndStats stats program "1."
    pure $ case logs of
        [] -> firstLog : nextLogs' 2 nextStats' program
        _  -> nextLogs stats program
