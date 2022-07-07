module Arguments where

import Data.Char 
import FileHandling
import CLILogFormat (formatLog)
import CLIStatsFormat (formatStats)
import CLIProgramFormat (formatProgram)
import EndgameLog (testLog)
import EndgameStats (bodyweight)
import GetLog
import AdvanceStats
import NextLogs

handleArguments :: [String] -> IO ()

handleArguments ["next"] = do
    stats <- readStats "profile"
    program <- readProgram "profile"
    let log = getLog "Next:" program stats
    putStrLn $ formatLog log


handleArguments ["next", logCount] 
    | all isDigit logCount = do
        stats <- readStats "profile"
        program <- readProgram "profile"
        let logs = take (read logCount) $ nextLogs stats program 1
        putStrLn $ unlines $ reverse $ map formatLog logs
    | otherwise = putStrLn invalidArgumentResponse
    

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
    

handleArguments ["lifts"] =
    readStats "profile" >>=
    putStrLn . formatStats


handleArguments ["program"] =
    readProgram "profile" >>=
    putStrLn . formatProgram

handleArguments ["bw"] =
    readStats "profile" >>=
    putStrLn . (++ "kg") . show . bodyweight 

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