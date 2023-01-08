module Program.Format
    ( format ) where

import Program.Program
    ( Program
    , liftGroupCycles
    , liftList
    , isBodyweight
    , progression
    , liftCycle
    )
import Program.LiftGroupCycle
    ( LiftGroupCycle )
import Types.General
    ( Lift )
import Program.Session
    ( Session )
import Program.Set
    ( Set
    , SetType
        (..)
    , lift
    , reps
    , percent
    , setType
    )
import Data.Maybe
    ( fromJust )
import Data.List
    ( group )

format :: Program -> String
format program =
    init $ unlines $
    (formatLiftGroupCycles $ liftGroupCycles program) :
    "Lift cycles:" :
    (formatLift program <$> liftList program)

formatLiftGroupCycles :: [LiftGroupCycle] -> String
formatLiftGroupCycles cycles =
    unlines $
    "Lift group cycles:" :
    (formatLiftGroupCycle <$> cycles)

formatLiftGroupCycle :: LiftGroupCycle -> String
formatLiftGroupCycle cycle =
    "-> " ++
    (unwords $ (++ " ->") <$> cycle)

formatLift :: Program -> Lift -> String
formatLift program lift =
    init $ unlines $
    (init $ unwords $ [lift, progressionStr, maybeBodyweight]) :
    (formatSession =<< (fromJust $ liftCycle lift program))
  where
    progressionStr =
        "(+" ++ (show $ fromJust $ progression lift program) ++ "kg)"
    maybeBodyweight = if fromJust $ isBodyweight lift program
        then "(Bodyweight) "
        else ""

formatSession :: Session -> [String]
formatSession session =
    ("    - " ++ headGroup) :
    (("      " ++) <$> tailGroups)
  where
    headGroup : tailGroups =
        formatSetGroup <$> group session

formatSetGroup :: [Set] -> String
formatSetGroup sets =
    (lift set) ++ " " ++
    (show $ length sets) ++ "x" ++
    (show $ reps set) ++ " " ++
    maybePercent
  where
    set = head sets
    maybePercent = case setType set of
        PR   -> "PR"
        Work -> (show $ percent set) ++ "%"
