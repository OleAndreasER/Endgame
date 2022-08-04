module CurrentLog (currentLog, CurrentLog.lifts) where

import Types.Log as Log
import Types.Program as Program
import Types.Stats as Stats
import Types.General as General

currentLog :: Program -> Stats -> String -> Log
currentLog program stats label = Log
    { label = label
    , liftSessions = 
        map (liftSession program stats)
        $ CurrentLog.lifts program stats     
    }

liftSession :: Program -> Stats -> Lift -> LiftSession
liftSession program stats lift = LiftSession
    { Log.lift = lift
    , sets = map logSet'
        $ setsOfLift (statsOfLift stats lift)
        $ cycleOfLift lift program
    }
  where 
    logSet' =
        logSet (statsOfLift stats lift)
        $ bodyweight stats

setsOfLift :: LiftStats -> LiftCycle -> [Program.Set]
setsOfLift liftStats liftCycle = 
    sessions liftCycle !! (position . Stats.liftCycle) liftStats

logSet :: LiftStats -> Bodyweight -> Program.Set -> Log.Set
logSet liftStats bw set = Log.Set
    { Log.reps = Program.reps set
    , Log.weight = 
        roundTo (progression liftStats)
        $ pr * Program.percent set / 100 - maybeBodyweight
    , Log.setType = setType
    }
  where 
    setType = logSetType $ Program.setType set 
    maybeBodyweight
        | isBodyweight liftStats = bw
        | otherwise              = 0
    pr = Stats.pr liftStats + maybeProgression
    maybeProgression = case Program.setType set of
        Program.PR   -> progression liftStats
        Program.Work -> 0
        
roundTo :: Float -> Float -> Float
roundTo multiple n = multiple * multiples
  where
    multiples =
        fromIntegral $ round (n / multiple) :: Float

logSetType :: Program.SetType -> Log.SetType
logSetType Program.PR = Log.PR True
logSetType _          = Log.Work

lifts :: Program -> Stats -> [Lift]
lifts program stats =
    map (uncurry liftInPosition)
    $ zip
        (liftGroupCycles program)
        (liftGroupPositions stats)

liftInPosition :: LiftGroupCycle -> CyclePosition -> Lift
liftInPosition cycle cyclePosition = 
    cycle !! position cyclePosition