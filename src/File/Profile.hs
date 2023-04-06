module File.Profile
    ( toProgram
    , toStats
    , toLog
    , readLogs
    , readLog
    , logCount
    , readProgram
    , readStats
    , setProgram
    , setStats
    , setLogs
    , setProfile
    , readProfile
    , toProfile
    ) where

import Profile.Profile
    ( Profile
    , profile
    , program
    , stats
    , logs
    )
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
    logs <- readAllLogs
    setLogs $ toLog' logs n f
  where
    toLog' :: [Log] -> Int -> (Log -> Log) -> [Log]
    toLog' (log:logs) n f
        | null logs  = []
        | n == 0     = f log : logs
        | otherwise  = log : toLog' logs (n-1) f

readStats :: IO Stats
readStats = decodeFile =<< Path.stats =<< getProfile

readAllLogs :: IO [Log]
readAllLogs = decodeFile =<< Path.logs =<< getProfile

readLogs :: Int -> IO [Log]
readLogs logCount = take logCount <$> readAllLogs

logCount :: IO Int
logCount = length <$> readAllLogs

(!!?) :: [a] -> Int -> Maybe a
xs !!? i
    | (i > -1) && (length xs > i) = Just (xs !! i)
    | otherwise = Nothing

readLog :: Int -> IO (Maybe Log)
readLog n = (!!? n) <$> readAllLogs

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

setProfile :: Profile -> IO ()
setProfile profile = do
    setProgram $ program profile
    setStats $ stats profile
    setLogs $ logs profile

readProfile :: IO Profile
readProfile =
    profile <$>
    readProgram <*>
    readStats <*>
    readAllLogs

toProfile :: (Profile -> Profile) -> IO ()
toProfile f = readProfile >>= setProfile . f
