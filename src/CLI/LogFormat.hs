module CLI.LogFormat (formatLog) where
import Types.EndgameLog
import Data.List (group)

formatLog :: Log -> String
formatLog log = 
   (date log) ++ "\n" ++ (formatLiftSessions $ liftSessions log)

formatLiftSessions :: [LiftSession] -> String
formatLiftSessions liftSessions = 
    unlines
    $ map ("    " ++)
    $ foldr (++) []
    $ map formatLiftSession
    $ liftSessions

formatLiftSession :: LiftSession -> [String]
    --A LiftSession can contain multiple groups of sets
    --eg. ["Bench PR 1x3 100kg", "Bench Work 1x5 87kg"]
formatLiftSession liftSession = 
    map ((lift liftSession ++ " ") ++)
    $ map formatSetGroups
    $ group
    $ sets liftSession

formatSetGroups :: [Set] -> String
formatSetGroups sets =
    setTypeStr++" "++setsStr++"x"++repsStr++" "++weightStr++"kg"++failureStr
    where Set reps weight setType = head sets
          setTypeStr = head $ words $ show setType
          setsStr = show $ length sets
          repsStr = show reps
          weightStr = show weight
          failureStr = failure setType

failure :: SetType -> String
failure (PR False) = " (FAIL)"
failure _          = ""


{-
06/05/2022:
    Bench PR 1x3 100kg
    Bench Work 1x5 87kg 
    Squat PR 1x3 200kg (FAIL)
    Squat Work 1x5 170kg
    Row Work 3x5 75kg
-}
