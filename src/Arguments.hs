module Arguments where

import System.Environment
import Data.Char 

answerArguments :: IO ()
answerArguments = do
    args <- getArgs
    putStrLn $ answer args

answer :: [String] -> String
answer ["next"] = "Squats and stuff (big weight)"
answer ["list", logs] = if all isDigit logs then list else answer ["x"]
    where list = concat $ replicate n "Squats\n"
          n    = read logs
answer ["add"] = "Added Squats and stuff"
answer ["stats"] = "Squat 3RM: 300kg"
answer ["bw"] = "Your bodyweight is now 100kg (real man)"
answer ["help"] = "try next, list n, add, stats, bw"
answer _ = "Not a real argument. Try 'endgame2 help'"
