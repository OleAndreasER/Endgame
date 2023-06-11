module CLI.Arguments where

import CLI.Endgame.NextLog
    ( displayNextLog
    , displayNextLogs
    )
import CLI.Endgame.Log
    ( displayLogs
    , displayLog
    )
import CLI.Endgame.Help
    ( displayHelp )
import CLI.Endgame.Add
    ( addNextLog )
import CLI.Endgame.Bodyweight
    ( displayBodyweight
    , setBodyweight
    )
import CLI.Endgame.Profile
    ( createProfile
    , switchToProfile
    , displayProfiles
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
import CLI.ArgumentEnsuring
    ( ensurePositiveInt
    , ensureWeight
    , ensureCycle
    )
import CLI.Endgame.Remove (removeLog)
import CLI.Endgame.Fail (failLift)

handleArguments :: [String] -> IO ()

--HELP
handleArguments ["help"] = displayHelp

--PROFILE
handleArguments ["new", "profile"] = createProfile

handleArguments ["profile", profileName] = switchToProfile profileName

handleArguments ["profiles"] = displayProfiles

--LOGS
handleArguments ["logs", nStr] = ensurePositiveInt nStr displayLogs

handleArguments ["logs"] = displayLogs 1

handleArguments ["log", nStr] = ensurePositiveInt nStr displayLog

handleArguments ["log"] = displayLog 1

handleArguments ["remove", "log", nStr] =
   ensurePositiveInt nStr removeLog

handleArguments ["remove", "log"] = removeLog 1

handleArguments ["fail", lift] = failLift lift

--LIFTS
handleArguments ["lifts"] = displayLifts

handleArguments ["pr", lift, weightStr] =
    ensureWeight weightStr $ setPr lift

handleArguments ["cycle", lift, posStr, lenStr] =
    ensureCycle posStr lenStr $ \pos len ->
    setCycle lift (pos-1) len

handleArguments ["bodyweight"] = displayBodyweight

handleArguments ["bodyweight", bodyweightStr] = ensureWeight bodyweightStr setBodyweight

--PROGRAM
handleArguments ["program"] = displayProfileProgram
    
handleArguments ["progression", lift, weightStr] =
    ensureWeight weightStr $ setProgression lift

handleArguments ["toggle", "bodyweight", lift] = toggleBodyweight lift

--NEXT
handleArguments ["next"] = displayNextLog

handleArguments ["next", nStr] = ensurePositiveInt nStr displayNextLogs
    
--ADD
handleArguments ["add"] = addNextLog

handleArguments _ = putStrLn invalidArgumentResponse

invalidArgumentResponse :: String
invalidArgumentResponse = "Try 'endgame help'"
