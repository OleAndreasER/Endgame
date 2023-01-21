module CLI.Arguments where

import CLI.Endgame.NextLog
    ( displayNextLog
    , displayNextLogs
    )
import CLI.Endgame.Log
    ( displayLogs
    , displayLog
    --, failLiftInLog
    --, removeLog
    )
import CLI.Endgame.Help
    ( displayHelp
    , displayLiftsHelp
    , displayProgramHelp
    )
import CLI.Endgame.Add
    ( addNextLog )
import CLI.Endgame.Bodyweight
    ( displayBodyweight
    , setBodyweight
    )
import CLI.Endgame.Profile
    ( createNewProfile
    , switchToProfile
    )
import CLI.Endgame.Program
    ( displayProfileProgram
    , setProgression
    , toggleBodyweight
    --, displayLiftGroupCycle
    --, editLiftGroupCycle
    --, displayProgramLift
    --, editProgramLift
    ) 
import CLI.Endgame.Lifts
    ( displayLifts
    , setPr
    , setCycle
    )
import CLI.Endgame.Convert
    ( convertProfile )
import CLI.ArgumentEnsuring
    ( ensurePositiveInt
    , ensureWeight
    , ensureCycle
    )

handleArguments :: [String] -> IO ()

handleArguments ["help"] = displayHelp

handleArguments ["program", "help"] = displayProgramHelp

handleArguments ["profile", "new"] = createNewProfile

handleArguments ["profile", profile] = switchToProfile profile

handleArguments ["lifts", "help"] = displayLiftsHelp

handleArguments ["logs", nStr] = ensurePositiveInt nStr displayLogs

handleArguments ["logs"] = displayLogs 1

handleArguments ["log", nStr] = ensurePositiveInt nStr displayLog

handleArguments ["log"] = displayLog 1

handleArguments ["lifts"] = displayLifts

handleArguments ["pr", lift, weightStr] =
    ensureWeight weightStr $ setPr lift

handleArguments ["cycle", lift, posStr, lenStr] =
    ensureCycle posStr lenStr $ \pos len ->
    setCycle lift (pos-1) len

handleArguments ["bw"] = displayBodyweight

handleArguments ["bw", bodyweightStr] = ensureWeight bodyweightStr setBodyweight

handleArguments ["program"] = displayProfileProgram
    
handleArguments ["progression", lift, weightStr] =
    ensureWeight weightStr $ setProgression lift

handleArguments ["toggle-bodyweight", lift] = toggleBodyweight lift

--SCARY:
handleArguments ["convert"] = convertProfile

handleArguments _ = putStrLn invalidArgumentResponse

invalidArgumentResponse = "Try 'endgame help'"

{-

handleArguments ["next"] = displayNextLog

handleArguments ["next", nStr] = ensurePositiveInt nStr displayNextLogs
    
handleArguments ["add"] = addNextLog

handleArguments ["log", nStr, "fail", lift] = ensurePositiveInt nStr $ failLiftInLog lift

handleArguments ["log", nStr, "remove"] = ensurePositiveInt nStr removeLog

handleArguments ["program", "lift-group-cycle", nStr] =
    ensurePositiveInt nStr displayLiftGroupCycle

handleArguments ["program", "lift-group-cycle", nStr, "edit"] =
    ensurePositiveInt nStr editLiftGroupCycle

handleArguments ["program", "lift", lift] = displayProgramLift lift

handleArguments ["program", "lift", lift, "edit"] = editProgramLift lift
-}
