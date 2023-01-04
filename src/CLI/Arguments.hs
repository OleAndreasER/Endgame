module CLI.Arguments where

import CLI.Endgame.NextLog
    ( displayNextLog
    , displayNextLogs
    )
import CLI.Endgame.Log
    ( displayLogs
    , displayLog
    , failLiftInLog
    , removeLog
    , updateLifts -- temp location
    )
import CLI.Endgame.Help
    ( displayHelp
    , displayLiftsHelp
    , displayProgramHelp
    )
import CLI.Endgame.Add
    ( addNextLog
    )
import CLI.Endgame.Bodyweight
    ( displayBodyweight
    , setBodyweight
    )
import CLI.Endgame.Profile
    ( createNewProfile
    , switchToProfile
    )
import CLI.Endgame.Program
    ( displayProgram
    , displayLiftGroupCycle
    , editLiftGroupCycle
    , displayProgramLift
    , editProgramLift
    ) 
import CLI.Endgame.Lifts
    ( displayLifts
    , setPR
    , setProgression
    , setCycle
    , toggleBodyweight
    )
import CLI.ArgumentEnsuring
    ( ensurePositiveInt
    , ensureWeight
    , ensureCycle
    )

handleArguments :: [String] -> IO ()

handleArguments ["help"] = displayHelp

handleArguments ["lifts", "help"] = displayLiftsHelp

handleArguments ["program", "help"] = displayProgramHelp

handleArguments ["next"] = displayNextLog

handleArguments ["next", nStr] = ensurePositiveInt nStr displayNextLogs
    
handleArguments ["logs", nStr] = ensurePositiveInt nStr displayLogs

handleArguments ["logs"] = displayLogs 1

handleArguments ["add"] = addNextLog
    
handleArguments ["lifts"] = displayLifts

handleArguments ["lifts", "pr", lift, weightStr] =
    ensureWeight weightStr $ setPR lift

handleArguments ["lifts", "progression", lift, weightStr] =
    ensureWeight weightStr $ setProgression lift
    
handleArguments ["lifts", "cycle", lift, posStr, lenStr] =
    ensureCycle posStr lenStr $ \pos len ->
    setCycle lift (pos-1) len

handleArguments ["lifts", "toggle-bodyweight", lift] = toggleBodyweight lift

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
