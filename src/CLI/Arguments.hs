module CLI.Arguments where

import Data.Char 
import FileHandling
import CLI.ArgumentEnsuring
import CLI.LogFormat (formatLog)
import CLI.StatsFormat (formatStats)
import CLI.ProgramFormat (formatProgram, formatLiftGroupCycle)
import CLI.CreateProfile (createProfile)
import CLI.Edit.LiftGroupCycle (editLiftGroupCycle)
import Types.Log as Log
import Types.General 
import Types.Program as Program (liftGroupCycles, setLiftGroupCycle)
import Types.Stats as Stats
    ( LiftStats
    , setLiftGroupPosition
    , endingCycle
    , bodyweight
    , setPR
    , toLiftStats
    , setProgression
    , liftIsInStats
    , setCycle
    , toggleBodyweight
    , addProgressions
    )
import qualified Types.Stats as Stats (addWork)
import CurrentLog 
import NextLogs

handleArguments :: [String] -> IO ()

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
             \View your latest logs:\n\
             \  endgame logs\n\
             \  endgame logs {amount}\n\n\
             \View or edit a specific log:\n\
             \  endgame log\n\
             \  endgame log {n}\n\
             \  endgame log {n} fail {lift}\n\n\
             \View or set your bodyweight:\n\
             \  endgame bw\n\
             \  endgame bw {new bodyweight}\n\n\
             \View your lifts' stats:\n\
             \  endgame lifts\n\n\
             \Commands for editing your stats:\n\
             \  endgame lifts help\n\n\
             \View your program:\n\
             \  endgame program\n\n\
             \Commands for editing your program:\n\
             \  endgame program help\n"

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

handleArguments ["lifts", "help"] =
    putStrLn "Set pr for a lift:\n\
             \  endgame lifts pr {lift} {weight}\n\n\
             \Set progression increment:\n\
             \  endgame lifts progression {lift} {increment}\n\n\
             \Set cycle of lift:\n\
             \  endgame lifts cycle {lift} {position} {length}\n\n\
             \Toggle if a lift should be a bodyweight movement:\n\
             \  endgame toggle-bodyweight {lift}\n"

handleArguments ["lifts", "pr", lift, weightStr] =
    ensureWeight weightStr
    $ \weight -> updateLifts lift $ setPR weight

handleArguments ["lifts", "progression", lift, weightStr] =
    ensureWeight weightStr
    $ \weight -> updateLifts lift $ setProgression weight
    
handleArguments ["lifts", "cycle", lift, posStr, lenStr] =
    ensureCycle posStr lenStr
    $ \pos len -> updateLifts lift $ setCycle (pos-1) len

handleArguments ["lifts", "toggle-bodyweight", lift] =
    updateLifts lift toggleBodyweight

handleArguments ["bw"] = readStats >>= putStrLn . (++ "kg") . show . bodyweight 

handleArguments ["bw", bodyweightStr] =
    ensureWeight bodyweightStr $ \bw -> do
    readStats >>= setStats . \stats -> stats {bodyweight = bw}
    putStrLn ("Bodyweight: "++bodyweightStr++"kg")

handleArguments ["profile", "new"] = do
    putStrLn "Profile name:"
    name <- getLine
    isProfile <- elem name <$> getProfiles
    if isProfile
    then putStrLn $ "There is already a profile named '"++name++"'."
    else createProfile name

handleArguments ["profile", profile] = do
    isProfile <- elem profile <$> getProfiles
    if isProfile
    then do
        setProfile profile
        putStrLn $ "Profile: "++profile
    else
        putStrLn $ "There is no profile called '"++profile++"'."

handleArguments ["log", nStr] = 
    ensureLog nStr $ putStrLn . formatLog

handleArguments ["log"] = handleArguments ["log", "1"]

handleArguments ["log", nStr, "fail", lift] =
    ensureLog nStr $ \log -> do
    readLogs >>= setLogs . (toElem log $ Log.failLift lift)

    let newLog = Log.failLift lift log
    putStrLn $ formatLog newLog

    let setType = liftSetType lift newLog
    case setType of
        Nothing -> return ()
        Just Work -> putStrLn "You can't fail a work set."
        Just (PR True)  -> unfailLift lift
        Just (PR False) -> CLI.Arguments.failLift lift

handleArguments ["program", "help"] =
    putStrLn "View or edit a lift group cycle:\n\
             \  endgame program lift-group-cycle {n}\n\
             \  endgame program lift-group-cycle {n} edit\n"

handleArguments ["program"] = readProgram >>= putStrLn . formatProgram

handleArguments ["program", "lift-group-cycle", nStr] =
    ensurePositiveInt nStr $ \n -> do
    cycles <- liftGroupCycles <$> readProgram
    ensureIndex n cycles $ putStrLn . formatLiftGroupCycle

handleArguments ["program", "lift-group-cycle", nStr, "edit"] =
    ensurePositiveInt nStr $ \n ->
    liftGroupCycles <$> readProgram >>= \cycles ->
    ensureIndex n cycles $ \oldCycle -> do
    newCycle <- editLiftGroupCycle oldCycle
    readProgram >>= setProgram . setLiftGroupCycle (n-1) newCycle
    readStats >>= setStats . setLiftGroupPosition (n-1) (endingCycle $ length newCycle)



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

failLift :: Lift -> IO ()
failLift lift = do
    updateLifts lift (addProgressions (-2))
    putStrLn $ "Subtracted 2 progression's worth of weight from "++lift++"'s PR."
    addWork (-1) lift

unfailLift :: Lift -> IO ()
unfailLift lift = do
    updateLifts lift (addProgressions 2)
    putStrLn $ "Added back 2 progression's worth of weight to "++lift++"'s PR."
    addWork 1 lift

addWork :: Int -> Lift -> IO ()
addWork work lift = do
    putStrLn workTxt
    readStats >>= setStats . Stats.addWork work lift
  where 
    workTxt 
        | work == 0  = ""
        | work == 1  = "Added a work day to "++lift++"."
        | work == -1 = "Removed a work day from "++lift++"."
        | work > 1   = "Added "++show work++" work days to "++lift++"."
        | work < -1  = "Removed "++show work++" work days from "++lift++"."
