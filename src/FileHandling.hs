module FileHandling where

addLog :: String -> IO ()
addLog = appendFile "logs.txt"

nextLog :: String
nextLog = testLog

testLog :: String
testLog = "\n05/26/22:\n  PR Deadlift: 152.5kg 1/4\n  Volume Press: 48.75kg 2/3\n  Volume Chins: 16.25kg 2/5"