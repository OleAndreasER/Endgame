module CLI.Arguments where

import Data.Char 
import FileHandling
import Date (dateStr)
import CLI.ArgumentEnsuring
import CLI.LogFormat (formatLog)
import CLI.StatsFormat (formatStats)
import CLI.ProgramFormat
    (formatProgram
    , formatLiftGroupCycle
    , formatLiftCycle
    )
import CLI.CreateProfile (createProfile)
import CLI.Edit.LiftGroupCycle (editLiftGroupCycle)
import CLI.Edit.LiftCycle (editLiftCycle)
import Types.Log as Log
import Types.General 
import Types.Program as Program
    ( liftGroupCycles
    , setLiftGroupCycle
    , cycleOfLift
    )
import Types.Stats as Stats
    ( LiftStats
    , CyclePosition (CyclePosition)
    , setLiftGroupPosition
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
import Advance.PRs (regressPRs)
import Advance.Cycles (regressCycles)

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

handleArguments ["next"] = ifProfile $ do
    (nextLog', _) <- getNextLogAndStats "Next:"
    putStrLn $ formatLog nextLog'

handleArguments ["next", nStr] = ifProfile $
    ensurePositiveInt nStr $ \n -> do
    logs <- take n <$> getNextLogs
    putStrLn $ unlines $ reverse $ map formatLog logs
    
handleArguments ["logs", nStr] = ifProfile $
    ensurePositiveInt nStr $ \n ->
    readLogs >>= putStrLn . unlines . map formatLog . reverse . take n

handleArguments ["logs"] = handleArguments ["logs", "1"]

handleArguments ["add"] = ifProfile $ do
    (nextLog', nextStats') <- getNextLogAndStats =<< dateStr
    addLog nextLog'
    putStrLn $ "Added:\n" ++ formatLog nextLog'
    setStats nextStats'
    
handleArguments ["lifts"] =
    ifProfile $ readStats >>= putStrLn . formatStats

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
    ifProfile $ ensureWeight weightStr
    $ \weight -> updateLifts lift $ setPR weight

handleArguments ["lifts", "progression", lift, weightStr] =
    ifProfile $ ensureWeight weightStr
    $ \weight -> updateLifts lift $ setProgression weight
    
handleArguments ["lifts", "cycle", lift, posStr, lenStr] =
    ifProfile $ ensureCycle posStr lenStr
    $ \pos len -> updateLifts lift $ setCycle (pos-1) len

handleArguments ["lifts", "toggle-bodyweight", lift] =
    ifProfile $ updateLifts lift toggleBodyweight

handleArguments ["bw"] =
    ifProfile $ readStats >>= putStrLn . (++ "kg") . show . bodyweight 

handleArguments ["bw", bodyweightStr] = ifProfile $
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

handleArguments ["log", nStr] = ifProfile $ 
    ensureLog nStr $ putStrLn . formatLog

handleArguments ["log"] = handleArguments ["log", "1"]

handleArguments ["log", nStr, "fail", lift] = ifProfile $
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

handleArguments ["log", "1", "remove"] =
    ifProfile $ ensureLog "1" $ \log -> do

    readLogs >>= setLogs . tail
    readStats >>= setStats . regressPRs log
    setStats =<< regressCycles <$> readProgram <*> readStats

    putStrLn "Removed:"
    putStrLn $ formatLog log
    putStrLn "After undoing PRs and cycle advances, this is your stats:"
    readStats >>= putStrLn . formatStats

handleArguments ["log", nStr, "remove"] = ifProfile $
    ensurePositiveInt nStr $ \n ->
    ensureLog nStr         $ \log -> do
    
    setLogs =<< removeAt (n - 1) <$> readLogs

    putStrLn "Removed:"
    putStrLn $ formatLog log

handleArguments ["program", "help"] =
    putStrLn "View or edit a lift group cycle:\n\
             \  endgame program lift-group-cycle {n}\n\
             \  endgame program lift-group-cycle {n} edit\n\n\
             \View or edit a lift:\n\
             \  endgame program lift {lift}\n\
             \  endgame program lift {lift} edit\n"

handleArguments ["program"] = ifProfile $
    readProgram >>= putStrLn . formatProgram

handleArguments ["program", "lift-group-cycle", nStr] =
    ifProfile $ ensurePositiveInt nStr $ \n -> do
    cycles <- liftGroupCycles <$> readProgram
    ensureIndex n cycles $ putStrLn . formatLiftGroupCycle

handleArguments ["program", "lift-group-cycle", nStr, "edit"] =
    ifProfile $ ensurePositiveInt nStr $ \n ->
    liftGroupCycles <$> readProgram >>= \cycles ->
    ensureIndex n cycles $ \oldCycle -> do
    newCycle <- editLiftGroupCycle oldCycle
    readProgram >>= setProgram . setLiftGroupCycle (n-1) newCycle
    let resetCycle = CyclePosition 0 $ length newCycle
    readStats >>= setStats . setLiftGroupPosition (n-1) resetCycle

handleArguments ["program", "lift", lift] =
    ifProfile $ ifLift lift $
    putStrLn =<< formatLiftCycle . cycleOfLift lift <$> readProgram

handleArguments ["program", "lift", lift, "edit"] =
    ifProfile $ ifLift lift $ do
    editLiftCycle =<< cycleOfLift lift <$> readProgram
    putStrLn "hhh"

handleArguments _ = putStrLn invalidArgumentResponse

invalidArgumentResponse = "Try 'endgame help'"

--Applies f to the first instance of y in a list
toElem :: Eq a => a -> (a -> a) -> [a] -> [a]
toElem y f (x:xs)
    | y == x    = f x : xs
    | otherwise = x : toElem y f xs

removeAt :: Int -> [a] -> [a]
removeAt n xs 
    | n < length xs = left ++ right
    | otherwise     = xs
    where (left, (_ :right)) = splitAt n xs

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
    putStrLn $ "Subtracted 2 progression's worth of weight from "++lift++"'s PR."
    addWork 1 lift
    updateLifts lift (addProgressions (-2))

unfailLift :: Lift -> IO ()
unfailLift lift = do
    putStrLn $ "Added back 2 progression's worth of weight to "++lift++"'s PR."
    addWork (-1) lift
    updateLifts lift (addProgressions 2)

addWork :: Int -> Lift -> IO ()
addWork work lift = do
    putStrLn $ workTxt ++ "\n"
    readStats >>= setStats . Stats.addWork work lift
  where 
    workTxt 
        | work == 0  = ""
        | work == 1  = "Added a work day to "++lift++"."
        | work == -1 = "Removed a work day from "++lift++"."
        | work > 1   = "Added "++show work++" work days to "++lift++"."
        | work < -1  = "Removed "++show work++" work days from "++lift++"."
