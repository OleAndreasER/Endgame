module NextLogs
    ( nextLogs
    , nextLog
    , nextStats
    , nextLogAndStats
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
    nextLog' = currentLog program stats label
    advancedPRs = advancePRs nextLog' stats
    nextStats' = advanceCycles program advancedPRs 

getNextLogAndStats :: String -> IO (Log, Stats)
getNextLogAndStats label = do
    stats <- readStats
    program <- readProgram
    pure $ nextLogAndStats stats program label

--make cooler
getNextLogs :: IO [Log]
getNextLogs = do
    stats <- readStats
    program <- readProgram
    pure $ nextLogs stats program
