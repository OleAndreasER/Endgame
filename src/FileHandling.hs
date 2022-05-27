module FileHandling where

import System.Directory (createDirectoryIfMissing)

addLog :: String -> IO ()
addLog = appendFile "endgame-profiles/first-profile/logs.txt"

nextLog :: String
nextLog = testLog

testLog :: String
testLog = "\n05/26/22:\n  PR Deadlift: 152.5kg 1/4\n  Volume Press: 48.75kg 2/3\n  Volume Chins: 16.25kg 2/5"

--A profile contains training logs and stats. 
createProfile :: String -> IO ()
createProfile profileName =
    createDirectoryIfMissing True ("endgame-profiles/" ++ profileName)
