module Arguments where

import System.Environment
import Data.Char 

answerArguments :: IO ()
answerArguments = do
    args <- getArgs
    addLog testLog
    putStrLn $ answer args

answer :: [String] -> String
answer ["next"] = testLog
answer ["list", logs] = if all isDigit logs then list else answer ["x"]
    where list = concat $ replicate n "Squats\n"
          n    = read logs
answer ["add"] = "Added:" ++ testLog
answer ["stats"] = "Squat 3RM: 300kg"
answer ["bw"] = "Your bodyweight is now 100kg (real man)"
answer ["help"] = "try next, list n, add, stats, bw"
answer _ = "Not a real argument. Try 'endgame2 help'"

testLog = "\n05/26/22:\n  PR Deadlift: 152.5kg 1/4\n  Volume Press: 48.75kg 2/3\n  Volume Chins: 16.25kg 2/5"

addLog = appendFile "logs.txt"