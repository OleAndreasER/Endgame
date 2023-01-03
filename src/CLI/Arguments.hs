module CLI.Arguments where

import Endgame.NextLog
    ( displayNextLog
    , displayNextLogs
    )
import Endgame.Log
    ( displayLogs
    , displayLog
    , failLiftInLog
    , removeLog
    , updateLifts -- temp location
    )

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
import qualified Types.Program as Program
    ( lift 
    )
import Types.Program as Program
    ( liftGroupCycles
    , setLiftGroupCycle
    , integrateLiftCycle
    , cycleOfLift
    )
import Types.Stats as Stats
    ( LiftStats
    , CyclePosition (CyclePosition)
    , setLiftGroupPosition
    , bodyweight
    , renameLift
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

handleArguments ["next"] = displayNextLog

handleArguments ["next", nStr] = ensurePositiveInt nStr displayNextLogs
    
handleArguments ["logs", nStr] = ensurePositiveInt nStr displayLogs

handleArguments ["logs"] = displayLogs 1

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

handleArguments ["log", nStr] = ensurePositiveInt nStr displayLog

handleArguments ["log"] = displayLog 1

handleArguments ["log", nStr, "fail", lift] = ensurePositiveInt nStr $ failLiftInLog lift

handleArguments ["log", nStr, "remove"] = ensurePositiveInt nStr removeLog

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
    edited <- editLiftCycle =<< cycleOfLift lift <$> readProgram
    readProgram >>= setProgram . integrateLiftCycle lift edited
    readStats >>= setStats . renameLift lift (Program.lift edited)

handleArguments _ = putStrLn invalidArgumentResponse

invalidArgumentResponse = "Try 'endgame help'"
