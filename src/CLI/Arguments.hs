module CLI.Arguments where

import Data.Char 
import FileHandling
import CLI.LogFormat (formatLog)
import CLI.StatsFormat (formatStats)
import CLI.ProgramFormat (formatProgram)
import Types.Log (Log, testLog, did, failLift)
import Types.Stats (LiftStats, bodyweight, addWork, setPR, toLiftStats, setProgression, liftIsInStats, setCycle, toggleBodyweight)
import CurrentLog 
import Advance.Stats
import NextLogs
import Text.Read (readMaybe)

handleArguments :: [String] -> IO ()

handleArguments ["next"] = do
    stats <- readStats
    program <- readProgram
    let log = currentLog program stats "Next:"
    putStrLn $ formatLog log


handleArguments ["next", logCountStr] =
    handleIfInt logCountStr 
    $ handleIf (> 0) 
    (\logCount -> do
        stats <- readStats
        program <- readProgram
        let logs = take logCount $ nextLogs stats program
        putStrLn $ unlines $ reverse $ map formatLog logs)
    

handleArguments ["logs", logCountStr] =
    handleIfInt logCountStr 
    $ handleIf (> 0)
    (\logCount -> readLogs >>= 
    putStrLn . latestLogs logCount . map formatLog)

handleArguments ["logs"] = handleArguments ["logs", "1"]


handleArguments ["add"] = do
    stats <- readStats
    program <- readProgram
    let log = currentLog program stats "date" 
    addLog log
    putStrLn $ "Added:\n" ++ formatLog log
    setStats $ advanceStats log stats
    
handleArguments ["lifts"] = readStats >>= putStrLn . formatStats

handleArguments ["lifts", "pr", lift, weightStr] =
    handleIfFloat weightStr
    $ handleIf (>= 0) (\weight -> updateLifts lift (setPR weight))

handleArguments ["lifts", "progression", lift, weightStr] =
    handleIfFloat weightStr
    $ handleIf (>= 0) (\weight -> updateLifts lift (setProgression weight))
    
handleArguments ["lifts", "cycle", lift, posStr, lenStr] =
    handleIfInt posStr
    $ handleIf (> 0) (\pos ->
    handleIfInt lenStr
    $ handleIf (> 0)
    $ messagedHandleIf (>= pos) ("Cycle position out of bounds")
    (\len -> updateLifts lift (setCycle (pos-1) len)))

handleArguments ["lifts", "toggle-bodyweight", lift] =
    updateLifts lift toggleBodyweight

handleArguments ["program"] = readProgram >>= putStrLn . formatProgram

handleArguments ["bw"] = readStats >>= putStrLn . (++ "kg") . show . bodyweight 

handleArguments ["bw", bodyweightStr] =
    handleIfFloat bodyweightStr 
    $ handleIf (>= 0) (\bw -> do
        readStats >>= setStats . (\stats -> stats {bodyweight = bw})
        putStrLn ("Bodyweight: "++bodyweightStr++"kg"))
   

--TODO
handleArguments ["profile", "new"] =
    createProfile "profile" "everyotherday"

handleArguments ["profile", profile] = do
    setProfile profile
    putStrLn ("Profile: "++profile)

handleArguments ["log", nStr] = 
    handleIfInt nStr 
    $ handleIf (> 0)
    $ withLog $ putStrLn . formatLog

handleArguments ["log"] = handleArguments ["log", "1"]

handleArguments ["log", nStr, "fail", lift] =
    handleIfInt nStr
    $ handleIf (> 0)
    $ withLog 
    $ messagedHandleIf (did lift) ("You didn't do "++lift) (\log -> do
        readLogs >>= setLogs . (toElem log $ failLift lift)
        readStats >>= setStats . addWork lift -- fix
        putStrLn . formatLog . failLift lift $ log)
    

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

handleArguments _ = putStrLn invalidArgumentResponse

invalidArgumentResponse = "Try 'endgame help'"

handleIfFloat :: String -> (Float -> IO ()) -> IO ()
handleIfFloat str f = case readMaybe str :: Maybe Float of
    Nothing -> putStrLn invalidArgumentResponse 
    Just x  -> f x

handleIfInt :: String -> (Int -> IO ()) -> IO ()
handleIfInt str f = case readMaybe str :: Maybe Int of
    Nothing -> putStrLn invalidArgumentResponse 
    Just x  -> f x

handleIf :: (a -> Bool) -> (a -> IO ()) -> a -> IO ()
handleIf predicate = messagedHandleIf predicate invalidArgumentResponse

messagedHandleIf :: (a -> Bool) -> String -> (a -> IO ()) -> a -> IO ()
messagedHandleIf predicate msg f x
    | predicate x = f x 
    | otherwise   = putStrLn msg

latestLogs :: Int -> [String] -> String
latestLogs n logs = unlines $ reverse $ take m logs
    where m = min n $ length logs

withLog :: (Log -> IO ()) -> Int -> IO ()
withLog f n = do
    logs <- readLogs    
    if n > length logs
        then putStrLn ("There are only "++(show $ length logs)++" logs")
        else f $ logs !! (n-1)

--Applies f to the first instance of y in a list
toElem :: Eq a => a -> (a -> a) -> [a] -> [a]
toElem y f (x:xs)
    | y == x    = f x : xs
    | otherwise = x : toElem y f xs

updateLifts :: String -> (LiftStats -> LiftStats) -> IO ()
updateLifts lift f =
    readStats >>=
    messagedHandleIf (liftIsInStats lift) ("You don't do "++lift++".") (\_ -> do 
        readStats >>= setStats . (toLiftStats f lift)
        readStats >>= putStrLn . formatStats)