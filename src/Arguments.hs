module Arguments where

import Data.Char 
import FileHandling
import CLILogFormat (formatLog)
import CLIProgramFormat (formatProgram)
import EndgameLog (testLog)
import GetLog
import AdvanceStats

handleArguments :: [String] -> IO ()

handleArguments ["next"] = do
    stats <- readStats "profile"
    program <- readProgram "profile"
    let log = getLog "date" program stats
    putStrLn $ formatLog log

handleArguments ["list", logCount] =
    readLogs "profile" >>=
    putStrLn . latestLogs logCount . map formatLog

handleArguments ["list"] = handleArguments ["list", "1"]

handleArguments ["add"] = do
    stats <- readStats "profile"
    program <- readProgram "profile"
    let log = getLog "date" program stats

    addLog "profile" log
    putStrLn $ "Added:\n" ++ formatLog log
    
    setStats "profile" $ advanceStats stats log
    

--TODO: format stats
handleArguments ["stats"] =
    putStrLn "Squat 3RM: 300kg"

handleArguments ["program"] =
    readProgram "standard-everyotherday.txt" >>=
    putStrLn . formatProgram

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