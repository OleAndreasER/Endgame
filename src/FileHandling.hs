module FileHandling where

import System.Directory (createDirectoryIfMissing)
import Data.Binary
import EndgameProgram (Program)
import EndgameLog (Log, testLog)


--temp
addLog :: String -> IO ()
addLog _ = encodeFile "endgame-profiles/first-profile/logs.txt" testLog

nextLog = "hhh"

readLog :: IO Log
readLog = decodeFile "endgame-profiles/first-profile/logs.txt"

readProgram :: String -> IO Program
readProgram programName = decodeFile ("endgame-programs/" ++ programName)

--A profile contains training logs and stats. 
createProfile :: String -> IO ()
createProfile profileName =
    createDirectoryIfMissing True ("endgame-profiles/" ++ profileName)
