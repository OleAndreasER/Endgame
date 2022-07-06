module FileHandling where

import System.Directory (createDirectoryIfMissing)
import Data.Binary
import EndgameProgram (Program)
import EndgameLog (Log, testLog)
import EndgameStats (Stats)


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
readProgram programName =
    decodeFile ("endgame-programs/"++programName)

--A profile contains training logs, stats and program. 
createProfile :: String -> String -> IO ()
createProfile profile programName = do
    createDirectoryIfMissing True directory
    program <- readProgram programName
    encodeFile (directory ++ "/program.txt") program
    where directory = "endgame-profiles/" ++ profile

