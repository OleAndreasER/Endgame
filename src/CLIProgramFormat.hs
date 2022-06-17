module CLIProgramFormat (formatProgram) where

import EndgameProgram
import EndgameGeneralTypes (Percent)
import Data.List (group, intercalate)

formatLiftGroupCycle :: LiftGroupCycle -> String
formatLiftGroupCycle (LiftGroupCycle lifts) =
    unwords
    (map ("-> " ++) lifts)
    ++ " ->" 

formatLiftGroupCycles :: [LiftGroupCycle] -> String
formatLiftGroupCycles cycles = 
    (++) "Lift group cycles:\n"
    $ unlines 
    $ map formatLiftGroupCycle cycles

--5x87.0%
formatSet :: Set -> String
formatSet (Set reps percent setType) =
    (show reps) ++"x" ++ formatPercent setType percent

formatPercent :: SetType -> Percent -> String
formatPercent PR _ = "PR"
formatPercent _ percent = show percent ++ "%"

formatSetGroup :: [Set] -> String 
formatSetGroup sets = 
    (++) ((show $ length sets)++"x")
    $ formatSet
    $ head sets

formatSession :: [Set] -> String
formatSession sets =
    (++) " - "
    $ intercalate "\n           "
    $ map formatSetGroup
    $ group sets

formatSessionCycle :: [[Set]] -> String
formatSessionCycle cycle =
    intercalate "\n        "
    $ map formatSession cycle

formatLiftCycle :: LiftCycle -> String
formatLiftCycle cycle =
    unlines [liftStr, prStr, workStr]
    where liftStr = lift cycle
          prStr = "    PR  " ++ (formatSession $ prSession cycle)
          workStr = "    Work" ++ (formatSessionCycle $ workSessionCycle cycle)

formatProgram :: Program -> String
formatProgram (Program liftGroupCycles liftCycles) = 
    (++) (formatLiftGroupCycles liftGroupCycles++"\n")
    (concat $ map formatLiftCycle liftCycles)


{- 
Lift group cycles:
-> Press -> Bench ->
-> Squat -> Squat -> Deadlift ->
-> Chin -> Row ->

Lift cycles:
Press
    PR   - 1x3xPR
           1x5x87% 
    Work - 3x5x87%
           1x5x87% 
         - 3x7x80%  [This is not in everyotherday]
Bench
    PR   - 1x3xPR
           1x5x87% 
    Work - 3x5x87%
Squat
    PR   - 1x3xPR
           1x5x87% 
    Work - 3x5x87%
Deadlift
    PR   - 1x3xPR
    Work - 2x5x87%
Chin
    PR   - 1x3xPR
           2x5x87% 
    Work - 4x5x87%
Row
    PR   - 1x3xPR
           2x5x87% 
    Work - 4x5x87%
-}