module File.Profile
    ( toProgram
    , toStats
    , toLog
    , readLogs
    , readLog
    , readProgram
    , readStats
    , setProgram
    , setStats
    , setLogs
    , addLog
    ) where

import Program.Program
    ( Program )
import Stats.Stats
    ( Stats )
import Log.Log
    ( Log )
import Data.Binary
    ( Binary
    , encodeFile
    , decodeFile
    )
import qualified File.Path as Path
    ( program
    , logs
    , stats
    )
import File.ProfileManagement
    ( getProfile )

toProgram :: (Program -> Program) -> IO ()
toProgram f = readProgram >>= setProgram . f

toStats :: (Stats -> Stats) -> IO ()
toStats f = readStats >>= setStats . f

toLog :: Int -> (Log -> Log) -> IO ()
toLog n f = do
    logs <- readLogs
    setLogs $ toLog' logs n f
  where
    toLog' :: [Log] -> Int -> (Log -> Log) -> [Log]
    toLog' (log:logs) n f
        | logs == [] = []
        | n == 0     = f log : logs
        | otherwise  = log : toLog' logs (n-1) f

readStats :: IO Stats
readStats = decodeFile =<< Path.stats =<< getProfile

readLogs :: IO [Log]
readLogs = decodeFile =<< Path.logs =<< getProfile

(!!?) :: [a] -> Int -> Maybe a
xs !!? i
    | (i > -1) && (length xs > i) = Just (xs !! i)
    | otherwise = Nothing

readLog :: Int -> IO (Maybe Log)
readLog n = (!!? n) <$> readLogs

readProgram :: IO Program
readProgram = decodeFile =<< Path.program =<< getProfile

encodeFile' :: Binary a => a -> FilePath -> IO ()
encodeFile' = flip encodeFile

setStats :: Stats -> IO ()
setStats stats = encodeFile' stats =<< Path.stats =<< getProfile

setLogs :: [Log] -> IO ()
setLogs logs = encodeFile' logs =<< Path.logs =<< getProfile 

setProgram :: Program -> IO ()
setProgram program = encodeFile' program =<< Path.program =<< getProfile

addLog :: Log -> IO ()
addLog log = do
    logs <- readLogs
    setLogs (log:logs)
