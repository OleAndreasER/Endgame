module NextLogs
    ( nextLogs
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
import Relude.Functor.Fmap ((??))

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

nextLogAndStats :: Stats -> Program -> String -> (Log, Stats)
nextLogAndStats stats program label =
    (nextLog', nextStats')
  where
    nextLog' = currentLog program stats label
    advancedPRs = advancePRs nextLog' stats
    nextStats' = advanceCycles program advancedPRs 

getNextLogAndStats :: String -> IO (Log, Stats)
getNextLogAndStats label =
    nextLogAndStats
    <$> readStats
    <*> readProgram
    ?? label

getNextLogs :: IO [Log]
getNextLogs = nextLogs <$> readStats <*> readProgram
