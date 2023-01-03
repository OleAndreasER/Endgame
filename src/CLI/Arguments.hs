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
import Endgame.Help
    ( displayHelp
    , displayLiftsHelp
    , displayProgramHelp
    )
import Endgame.Add
    ( addNextLog
    )
import Endgame.Bodyweight
    ( displayBodyweight
    , setBodyweight
    )
import Endgame.Profile
    ( createNewProfile
    , switchToProfile
    )
import Endgame.Program
    ( displayProgram
    , displayLiftGroupCycle
    , editLiftGroupCycle
    , displayProgramLift
    , editProgramLift
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

handleArguments ["help"] = displayHelp

handleArguments ["lifts", "help"] = displayLiftsHelp

handleArguments ["program", "help"] = displayProgramHelp

handleArguments ["next"] = displayNextLog

handleArguments ["next", nStr] = ensurePositiveInt nStr displayNextLogs
    
handleArguments ["logs", nStr] = ensurePositiveInt nStr displayLogs

handleArguments ["logs"] = displayLogs 1

handleArguments ["add"] = addNextLog
    
handleArguments ["lifts"] =
    ifProfile $ readStats >>= putStrLn . formatStats

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

handleArguments ["bw"] = displayBodyweight

handleArguments ["bw", bodyweightStr] = ensureWeight bodyweightStr setBodyweight

handleArguments ["profile", "new"] = createNewProfile

handleArguments ["profile", profile] = switchToProfile profile

handleArguments ["log", nStr] = ensurePositiveInt nStr displayLog

handleArguments ["log"] = displayLog 1

handleArguments ["log", nStr, "fail", lift] = ensurePositiveInt nStr $ failLiftInLog lift

handleArguments ["log", nStr, "remove"] = ensurePositiveInt nStr removeLog

handleArguments ["program"] = displayProgram

handleArguments ["program", "lift-group-cycle", nStr] =
    ensurePositiveInt nStr displayLiftGroupCycle

handleArguments ["program", "lift-group-cycle", nStr, "edit"] =
    ensurePositiveInt nStr editLiftGroupCycle

handleArguments ["program", "lift", lift] = displayProgramLift lift

handleArguments ["program", "lift", lift, "edit"] = editProgramLift lift

handleArguments _ = putStrLn invalidArgumentResponse

invalidArgumentResponse = "Try 'endgame help'"
