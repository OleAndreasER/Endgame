module FileHandling where

import System.Directory (createDirectoryIfMissing)
import Data.Binary
import EndgameProgram (Program)
import EndgameLog (Log, testLog)
import EndgameStats (Stats, testStats)


addLog :: String -> Log -> IO ()
addLog profile log = do
    logs <- readLogs profile
    encodeFile ("endgame-profiles/"++profile++"/logs.txt") (log:logs)

setStats :: String -> Stats -> IO ()
setStats profile stats =
    encodeFile ("endgame-profiles/"++profile++"/stats.txt") stats

readStats :: String -> IO Stats
readStats profile =
    decodeFile ("endgame-profiles/"++profile++"/stats.txt")

readLogs :: String -> IO [Log]
readLogs profile =
    decodeFile ("endgame-profiles/"++profile++"/logs.txt")

readProgram :: String -> IO Program
readProgram profile =
    decodeFile ("endgame-profiles/"++profile++"/program.txt")

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

