module Arguments where

import Data.Char 
import FileHandling (addLog, readProgram, readLogs)
import CLILogFormat (formatLog)
import CLIProgramFormat (formatProgram)
import EndgameLog (testLog)

handleArguments :: [String] -> IO ()

--TODO
handleArguments ["next"] =
    readLogs "first-profile" >>=
    putStrLn . formatLog . head

handleArguments ["list", logCount] =
    readLogs "first-profile" >>=
    putStrLn . latestLogs logCount . map formatLog

handleArguments ["list"] = handleArguments ["list", "1"]

--TODO: testLog -> nextLog
handleArguments ["add"] = do
    addLog "first-profile" testLog
    putStrLn $ "Added:" ++ (formatLog testLog)

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