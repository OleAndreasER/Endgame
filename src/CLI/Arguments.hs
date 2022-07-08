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
import Text.Read (readMaybe)

handleArguments :: [String] -> IO ()

handleArguments ["next"] = do
    stats <- readStats
    program <- readProgram
    let log = getLog "Next:" program stats
    putStrLn $ formatLog log


handleArguments ["next", logCountStr] =
    handleIfInt logCountStr (\logCount -> do
        stats <- readStats
        program <- readProgram
        let logs = take logCount $ nextLogs stats program 1
        putStrLn $ unlines $ reverse $ map formatLog logs)
    

handleArguments ["list", logCountStr] =
    handleIfInt logCountStr (\logCount -> readLogs >>= 
    putStrLn . latestLogs logCount . map formatLog)

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

handleArguments ["bw", bodyweightStr] =
    handleIfFloat bodyweightStr (\bw -> do
        stats <- readStats
        setStats $ stats {bodyweight = bw}
        putStrLn ("Bodyweight: "++bodyweightStr++"kg"))
   

--TODO
handleArguments ["profile", "new"] =
    putStrLn invalidArgumentResponse

handleArguments ["profile", profile] = do
    setProfile profile
    putStrLn ("Profile: "++profile)



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

invalidArgumentResponse = "Try 'endgame help'"

handleIfFloat :: String -> (Float -> IO ()) -> IO ()
handleIfFloat str f = case readMaybe str :: Maybe Float of
    Nothing -> putStrLn invalidArgumentResponse 
    Just x  -> f x

handleIfInt :: String -> (Int -> IO ()) -> IO ()
handleIfInt str f = case readMaybe str :: Maybe Int of
    Nothing -> putStrLn invalidArgumentResponse 
    Just x  -> f x

latestLogs :: Int -> [String] -> String
latestLogs n logs = unlines $ take m logs
    where m = min n $ length logs
