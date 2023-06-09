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
    ( createProfile
    , switchToProfile
    )
import CLI.Endgame.Program
    ( displayProfileProgram
    , setProgression
    , toggleBodyweight
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
import CLI.Endgame.Remove (removeLog)

handleArguments :: [String] -> IO ()

handleArguments ["help"] = displayHelp

handleArguments ["program", "help"] = displayProgramHelp

handleArguments ["new", "profile"] = createProfile

handleArguments ["profile", profileName] = switchToProfile profileName

handleArguments ["lifts", "help"] = displayLiftsHelp

handleArguments ["logs", nStr] = ensurePositiveInt nStr displayLogs

handleArguments ["logs"] = displayLogs 1

handleArguments ["log", nStr] = ensurePositiveInt nStr displayLog

handleArguments ["log"] = displayLog 1

handleArguments ["remove", "log", nStr] =
   ensurePositiveInt nStr removeLog

handleArguments ["remove", "log"] = removeLog 1

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

handleArguments ["next"] = displayNextLog

handleArguments ["next", nStr] = ensurePositiveInt nStr displayNextLogs
    
handleArguments ["add"] = addNextLog

handleArguments _ = putStrLn invalidArgumentResponse

invalidArgumentResponse :: String
invalidArgumentResponse = "Try 'endgame help'"

{-
handleArguments ["log", nStr, "fail", lift] = ensurePositiveInt nStr $ failLiftInLog lift

handleArguments ["program", "lift-group-cycle", nStr] =
    ensurePositiveInt nStr displayLiftGroupCycle

handleArguments ["program", "lift-group-cycle", nStr, "edit"] =
    ensurePositiveInt nStr editLiftGroupCycle

handleArguments ["program", "lift", lift] = displayProgramLift lift

handleArguments ["program", "lift", lift, "edit"] = editProgramLift lift
-}
