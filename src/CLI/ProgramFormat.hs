module CLI.ProgramFormat (formatProgram) where

import Types.Program
import Types.General (Percent)
import Data.List (group, intercalate)

formatLiftGroupCycle :: LiftGroupCycle -> String
formatLiftGroupCycle lifts =
    unwords
    (map ("-> " ++) lifts)
    ++ " ->" 

formatLiftGroupCycles :: [LiftGroupCycle] -> String
formatLiftGroupCycles cycles = 
    (++) "Lift group cycles:\n"
    $ unlines 
    $ map formatLiftGroupCycle cycles

formatSet :: Set -> String
formatSet (Set reps percent setType) =
    show reps ++"x"++ formatPercent setType percent

formatPercent :: SetType -> Percent -> String
formatPercent PR _      = "PR"
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
  where 
    liftStr = lift cycle
    prStr =
        "    PR  " ++ (formatSession . prSession) cycle
    workStr =
        "    Work" ++ (formatSessionCycle . workSessionCycle) cycle

formatProgram :: Program -> String
formatProgram (Program liftGroupCycles liftCycles) = 
    (++) 
        (formatLiftGroupCycles liftGroupCycles++"\n")
        (concat $ map formatLiftCycle liftCycles)
