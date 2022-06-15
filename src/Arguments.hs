module Arguments where

import Data.Char 
import FileHandling (addLog, readProgram, readLogs)
import CLIFormating (formatLog)
import EndgameLog (testLog)

handleArguments :: [String] -> IO ()

--TODO
handleArguments ["next"] = do
    logs <- readLogs "first-profile"
    putStrLn $ formatLog $ head logs

handleArguments ["list", logCount] = do
    logs <- readLogs "first-profile"
    putStrLn $ latestLogs logCount $ map formatLog logs

handleArguments ["list"] = handleArguments ["list", "1"]

--TODO: testLog -> nextLog
handleArguments ["add"] = do
    addLog "first-profile" testLog
    putStrLn $ "Added:" ++ (formatLog testLog)

--TODO: format stats
handleArguments ["stats"] =
    putStrLn "Squat 3RM: 300kg"

--TODO: format program
handleArguments ["program"] = do
    program <- readProgram "standard-everyotherday.txt"
    print program

--TODO
handleArguments ["bw"] =
    putStrLn "Your bodyweight is now 100kg (real man)"

--TODO
handleArguments ["help"] =
    putStrLn "try next, list n, add, stats, bw"

handleArguments _ =
    putStrLn invalidArgumentResponse

invalidArgumentResponse = "Try 'endgame2 help'"


latestLogs :: String -> [String] -> String
latestLogs n logs
    | all isDigit n = unlines $ take m logs
    | otherwise     = invalidArgumentResponse
    where m = min (read n) (length logs)