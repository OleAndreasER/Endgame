module Arguments where

import Data.Char 

handleArguments :: [String] -> IO ()
handleArguments ["next"] = putStrLn testLog

handleArguments ["list", logs] = if all isDigit logs then list else handleArguments ["x"]
    where list = putStrLn $ concat $ replicate n "Squats\n"
          n    = read logs

handleArguments ["add"] = putStrLn $ "Added:" ++ testLog

handleArguments ["stats"] = putStrLn "Squat 3RM: 300kg"

handleArguments ["bw"] = putStrLn "Your bodyweight is now 100kg (real man)"

handleArguments ["help"] = putStrLn "try next, list n, add, stats, bw"

handleArguments _ = putStrLn "Not a real argument. Try 'endgame2 help'"

testLog :: String
testLog = "\n05/26/22:\n  PR Deadlift: 152.5kg 1/4\n  Volume Press: 48.75kg 2/3\n  Volume Chins: 16.25kg 2/5"

addLog :: String -> IO ()
addLog = appendFile "logs.txt"