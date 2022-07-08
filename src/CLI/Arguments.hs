module CLI.Arguments where

import Data.Char 
import FileHandling
import CLI.LogFormat (formatLog)
import CLI.StatsFormat (formatStats)
import CLI.ProgramFormat (formatProgram)
import Types.EndgameLog (testLog)
import Types.EndgameStats (bodyweight)
import GetLog
import AdvanceStats
import NextLogs

handleArguments :: [String] -> IO ()

handleArguments ["next"] = do
    stats <- readStats
    program <- readProgram
    let log = getLog "Next:" program stats
    putStrLn $ formatLog log


handleArguments ["next", logCount] 
    | all isDigit logCount = do
        stats <- readStats
        program <- readProgram
        let logs = take (read logCount) $ nextLogs stats program 1
        putStrLn $ unlines $ reverse $ map formatLog logs
    | otherwise = putStrLn invalidArgumentResponse
    

handleArguments ["list", logCount] =
    readLogs >>= putStrLn . latestLogs logCount . map formatLog

handleArguments ["list"] = handleArguments ["list", "1"]


handleArguments ["add"] = do
    stats <- readStats
    program <- readProgram
    let log = getLog "date" program stats

    addLog log
    putStrLn $ "Added:\n" ++ formatLog log
    
    setStats $ advanceStats stats log
    

handleArguments ["lifts"] =
    readStats >>= putStrLn . formatStats


handleArguments ["program"] =
    readProgram >>= putStrLn . formatProgram

handleArguments ["bw"] =
    readStats >>= putStrLn . (++ "kg") . show . bodyweight 

handleArguments ["help"] =
    putStrLn "Get started by creating a profile:\n\
             \  endgame profile new\n\n\
             \Switch profile:\n\
             \  endgame profile {name}\n\n\
             \View your first workout:\n\
             \  endgame next\n\
             \  endgame next {amount}\n\n\
             \Add it to your logs:\n\
             \  endgame add\n\n\
             \View your latest log:\n\
             \  endgame list\n\
             \  endgame list {amount}\n\n\
             \View your lifts' stats:\n\
             \  endgame lifts\n\n\
             \View your program:\n\
             \  endgame program\n\n\
             \View or set your bodyweight:\n\
             \  endgame bw\n\
             \  endgame bw {new bodyweight}"

handleArguments _ =
    putStrLn invalidArgumentResponse

invalidArgumentResponse = "Try 'endgame2 help'"


latestLogs :: String -> [String] -> String
latestLogs n logs
    | all isDigit n = unlines $ take m logs
    | otherwise     = invalidArgumentResponse
    where m = min (read n) (length logs)