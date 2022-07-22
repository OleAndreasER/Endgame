module CLI.Arguments where

import Data.Char 
import FileHandling
import CLI.LogFormat (formatLog)
import CLI.StatsFormat (formatStats)
import CLI.ProgramFormat (formatProgram)
import Types.General as General (Weight)
import Types.Log as Log
import Types.Stats as Stats (LiftStats, bodyweight, setPR, toLiftStats, setProgression, liftIsInStats, setCycle, toggleBodyweight)
import qualified Types.Stats as Stats (addWork)
import CurrentLog 
import NextLogs
import Text.Read (readMaybe)

handleArguments :: [String] -> IO ()

handleArguments ["next"] = do
    stats <- readStats
    program <- readProgram
    putStrLn . formatLog $ nextLog stats program "Next:"

handleArguments ["next", nStr] =
    ensurePositiveInt nStr $ \n -> do
    stats <- readStats
    program <- readProgram
    let logs = take n $ nextLogs stats program
    putStrLn $ unlines $ reverse $ map formatLog logs
    

handleArguments ["logs", nStr] =
    ensurePositiveInt nStr $ \n ->
    readLogs >>= putStrLn . unlines . map formatLog . reverse . take n

handleArguments ["logs"] = handleArguments ["logs", "1"]


handleArguments ["add"] = do
    stats <- readStats
    program <- readProgram
    let (nextLog', nextStats') = nextLogAndStats stats program "Date"
    addLog nextLog'
    putStrLn $ "Added:\n" ++ formatLog nextLog'
    setStats nextStats'
    

handleArguments ["lifts"] = readStats >>= putStrLn . formatStats

handleArguments ["lifts", "pr", lift, weightStr] =
    ensureWeight weightStr
    $ \weight -> updateLifts lift $ setPR weight

handleArguments ["lifts", "progression", lift, weightStr] =
    ensureWeight weightStr
    $ \weight -> updateLifts lift $ setProgression weight
    
--
handleArguments ["lifts", "cycle", lift, posStr, lenStr] =
    ensureCycle posStr lenStr
    $ \pos len -> updateLifts lift $ setCycle (pos-1) len

handleArguments ["lifts", "toggle-bodyweight", lift] =
    updateLifts lift toggleBodyweight


handleArguments ["program"] = readProgram >>= putStrLn . formatProgram


handleArguments ["bw"] = readStats >>= putStrLn . (++ "kg") . show . bodyweight 


handleArguments ["bw", bodyweightStr] =
    ensureWeight bodyweightStr $ \bw -> do
    readStats >>= setStats . \stats -> stats {bodyweight = bw}
    putStrLn ("Bodyweight: "++bodyweightStr++"kg")
   

handleArguments ["profile", "new"] = do
    putStrLn "Profile name:"
    name <- getLine
    createProfile name "everyotherday"

handleArguments ["profile", profile] = do
    setProfile profile
    putStrLn ("Profile: "++profile)


handleArguments ["log", nStr] = 
    ensureLog nStr $ putStrLn . formatLog

handleArguments ["log"] = handleArguments ["log", "1"]

handleArguments ["log", nStr, "fail", lift] =
    ensureLog nStr $ \log -> do
    readLogs >>= setLogs . (toElem log $ failLift lift)

    let newLog = failLift lift log
    putStrLn $ formatLog newLog

    let setType = liftSetType lift newLog
    case setType of
        Nothing -> return ()
        Just Work -> putStrLn "You can't fail a work set."
        Just (PR True)  -> addWork (-1) lift
        Just (PR False) -> addWork 1 lift
    

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

--Applies f to the first instance of y in a list
toElem :: Eq a => a -> (a -> a) -> [a] -> [a]
toElem y f (x:xs)
    | y == x    = f x : xs
    | otherwise = x : toElem y f xs

updateLifts :: String -> (LiftStats -> LiftStats) -> IO ()
updateLifts lift f = do
    stats <- readStats
    if liftIsInStats lift stats
    then do
        let newStats = toLiftStats f lift stats
        setStats newStats
        putStrLn $ formatStats newStats
    else
        putStrLn $ "You don't do "++lift++"."


addWork :: Int -> String -> IO ()
addWork work lift = do
    putStrLn workTxt
    readStats >>= setStats . Stats.addWork work lift
    where 
        workTxt | work == 0  = ""
                | work == 1  = "Added a work day to "++lift++"."
                | work == -1 = "Removed a work day from "++lift++"."
                | work > 1   = "Added "++show work++" work days to "++lift++"."
                | work < -1  = "Removed "++show work++" work days from "++lift++"."


--Converting and then ensuring valid arguments.

ensure :: Either String a -> (a -> IO ()) -> IO ()
ensure (Right x) f    = f x   
ensure (Left error) _ = putStrLn error

readFloat :: String -> Either String Float
readFloat str = case readMaybe str :: Maybe Float of
    Just n  -> Right n
    Nothing -> Left $ "'"++str++"' is not a number."

readInt :: String -> Either String Int
readInt str = case readMaybe str :: Maybe Int of
    Just n  -> Right n
    Nothing -> Left $ "'"++str++"' is not an integer."

check :: Show a => (a -> Bool) -> String -> a -> Either String a
check predicate aboutX x = if predicate x
    then Right x
    else Left $ "'"++ show x ++"' "++ aboutX 

getLog :: [Log] -> Int -> Either String Log
getLog logs n = if n > length logs
    then Left ("There are only "++(show $ length logs)++" logs.")
    else Right $ logs !! (n-1)

ensureWeight :: String -> (Weight -> IO ()) -> IO ()
ensureWeight str =
    ensure $ readFloat str >>= check (>= 0) "can't be negative." 

ensureNonNegativeInt :: String -> (Int -> IO ()) -> IO ()
ensureNonNegativeInt str =
    ensure $ readInt str >>= check (>= 0) "can't be negative."

ensurePositiveInt :: String -> (Int -> IO ()) -> IO ()
ensurePositiveInt str =
    ensure $ readInt str >>= check (> 0) "must be positive."

ensureCycle :: String -> String -> (Int -> Int -> IO ()) -> IO ()
ensureCycle posStr lenStr f =
    ensurePositiveInt posStr $ \pos ->
    ensurePositiveInt lenStr $ \len ->
    ensure (check (<= len) outOfBounds pos)
    $ \pos' -> f pos' len
    where 
        outOfBounds =
            "is larger than '"++lenStr++"'. Meaning it's out of the cycle's bounds."

ensureLog :: String -> (Log -> IO ()) -> IO ()
ensureLog nStr f = do
    logs <- readLogs
    ensure (readInt nStr >>= check (> 0) "must be positive." >>= getLog logs) f
