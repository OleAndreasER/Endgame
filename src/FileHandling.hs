module FileHandling where

import System.Directory (createDirectoryIfMissing)
import Data.Binary
import EndgameProgram (Program)
import EndgameLog (Log, testLog)
import EndgameStats (Stats, testStats)

readFromProfile file = do
    profile <- getProfile
    decodeFile ("endgame-profiles/" ++ profile ++ "/" ++ file)

setInProfile :: Binary a => String -> a -> IO ()
setInProfile file x = do
    profile <- getProfile  
    encodeFile ("endgame-profiles/" ++ profile ++ "/" ++ file) x

addLog :: Log -> IO ()
addLog log = do
    logs <- readLogs
    setInProfile "logs.txt" (log:logs)

setStats :: Stats -> IO ()
setStats = setInProfile "stats.txt"

readStats :: IO Stats
readStats = readFromProfile "stats.txt"

readLogs :: IO [Log]
readLogs = readFromProfile "logs.txt"

readProgram :: IO Program
readProgram = readFromProfile "program.txt"


readStandardProgram :: String -> IO Program
readStandardProgram programName =
    decodeFile ("endgame-programs/"++programName++".txt")

--A profile contains training logs, stats and program. 
createProfile :: String -> String -> IO ()
createProfile profile programName = do
    createDirectoryIfMissing True directory
    program <- readStandardProgram programName
    encodeFile (directory++"/program.txt") program
    encodeFile (directory++"/logs.txt") ([] :: [Log])
    encodeFile (directory++"/stats.txt") testStats
    where directory = "endgame-profiles/" ++ profile

setProfile :: String -> IO ()
setProfile = writeFile "profile.txt"
    
getProfile :: IO String
getProfile = readFile "profile.txt"