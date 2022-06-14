module Arguments where

import Data.Char 
import FileHandling (addLog, readProgram, readLogs)
import CLIFormating (formatLog)
import EndgameLog (testLog)

handleArguments :: [String] -> IO ()
handleArguments ["next"] = do
    logs <- readLogs
    putStrLn $ formatLog $ head logs

handleArguments ["list", amountOfLogs] =
    putStrLn $ listOfLatestLogs amountOfLogs

handleArguments ["add"] = do
    addLog testLog
    putStrLn $ "Added:" ++ (formatLog testLog)

handleArguments ["stats"] =
    putStrLn "Squat 3RM: 300kg"

handleArguments ["program"] = do
    program <- readProgram "standard-everyotherday.txt"
    print program

handleArguments ["bw"] =
    putStrLn "Your bodyweight is now 100kg (real man)"

handleArguments ["help"] =
    putStrLn "try next, list n, add, stats, bw"

handleArguments _ =
    putStrLn invalidArgumentResponse

invalidArgumentResponse = "Not a real argument. Try 'endgame2 help'"

--For "list n" argument
listOfLatestLogs :: String -> String
listOfLatestLogs amountOfLogs
    | strIsInteger amountOfLogs = list
    | otherwise                 = invalidArgumentResponse
    where list = concat $ replicate n "Squats\n"
          n    = read amountOfLogs

strIsInteger :: String -> Bool
strIsInteger = all isDigit
-- 