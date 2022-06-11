module FileHandling where

import System.Directory (createDirectoryIfMissing)
import Data.Binary
import EndgameProgram (Program)
import TrainingLog (Log)


addLog :: String -> IO ()
addLog = appendFile "endgame-profiles/first-profile/logs.txt"

nextLog :: String
nextLog = testLog2

testLog2 :: String
testLog2 = "\n05/26/22:\n  PR Deadlift: 152.5kg 1/4\n  Volume Press: 48.75kg 2/3\n  Volume Chins: 16.25kg 2/5"

readLog :: IO Log
readLog = decodeFile "endgame-profiles/first-profile/logs.txt"

readProgram :: String -> IO Program
readProgram programName = decodeFile ("endgame-programs/" ++ programName)

--A profile contains training logs and stats. 
createProfile :: String -> IO ()
createProfile profileName =
    createDirectoryIfMissing True ("endgame-profiles/" ++ profileName)
