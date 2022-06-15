module FileHandling where

import System.Directory (createDirectoryIfMissing)
import Data.Binary
import EndgameProgram (Program)
import EndgameLog (Log, testLog)


addLog :: Log -> IO ()
addLog log = do
    logs <- readLogs
    encodeFile "endgame-profiles/first-profile/logs.txt" (log:logs)

readLogs :: IO [Log]
readLogs = decodeFile "endgame-profiles/first-profile/logs.txt"

readProgram :: String -> IO Program
readProgram programName = decodeFile ("endgame-programs/" ++ programName)

--A profile contains training logs and stats. 
createProfile :: String -> IO ()
createProfile profileName =
    createDirectoryIfMissing True ("endgame-profiles/" ++ profileName)
