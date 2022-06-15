module FileHandling where

import System.Directory (createDirectoryIfMissing)
import Data.Binary
import EndgameProgram (Program)
import EndgameLog (Log, testLog)


addLog :: String -> Log -> IO ()
addLog profile log = do
    logs <- readLogs profile
    encodeFile ("endgame-profiles/"++profile++"/logs.txt") (log:logs)

readLogs :: String -> IO [Log]
readLogs profile = decodeFile ("endgame-profiles/"++profile++"/logs.txt")

readProgram :: String -> IO Program
readProgram programName = decodeFile ("endgame-programs/" ++ programName)

--A profile contains training logs and stats. 
createProfile :: String -> IO ()
createProfile profile =
    createDirectoryIfMissing True ("endgame-profiles/" ++ profile)

