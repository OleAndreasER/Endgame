module Arguments where

import Data.Char 
import FileHandling (addLog, readProgram, readLogs)
import CLIFormating (formatLog)
import EndgameLog (testLog, Log)

handleArguments :: [String] -> IO ()
handleArguments ["next"] = do
    logs <- readLogs
    putStrLn $ formatLog $ head logs

handleArguments ["list", n] = do
    logs <- readLogs
    putStrLn $ listOfLogs (read n) logs

handleArguments ["list"] = handleArguments ["list", "1"]

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


listOfLogs :: Int -> [Log] -> String
listOfLogs n logs = 
    unlines $ map formatLog (take m logs)
        where m = max n (length logs)