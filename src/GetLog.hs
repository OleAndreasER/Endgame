module GetLog (getLog) where 

import qualified Types.Log as Log
import qualified Types.Program as Program
import Types.GeneralTypes (Lift, Weight)
import Types.Stats 

{- getLog gets the current log from Stats,
   as specified by the Program. 
   It does not give the next log.
-}

getLog :: String
       -> Program.Program
       -> Stats
       -> Log.Log
getLog date (Program.Program liftGroupCycles liftCycles) stats = 
    Log.Log 
        { Log.date = date
        , Log.liftSessions = 
            map (liftSession liftCycles stats)
            $ map (uncurry $ liftInPosition)
            $ zip (liftGroupCycles)
            $ liftGroupPositions stats
        }

liftSession :: [Program.LiftCycle]
            -> Stats
            -> Lift
            -> Log.LiftSession
liftSession liftCycles stats lift = 
    Log.LiftSession
        { Log.lift = lift
        , Log.sets = map (logSet liftStats' (bodyweight stats)) session
        }
    where liftStats' = statsOfLift stats lift
          session = sessionOfLiftStats (cycleOfLift liftCycles lift)
                                       (statsOfLift stats lift)


liftInPosition :: Program.LiftGroupCycle
               -> CyclePosition
               -> Lift
liftInPosition cycle cyclePosition =  
    cycle !! (position cyclePosition)


logSet :: LiftStats -> Bodyweight -> Program.Set -> Log.Set
logSet liftStats' bodyweight (Program.Set reps percent setType) =
    Log.Set reps roundedWeight (logSetType setType)
    where pr' = pr liftStats'
          multiple = progression liftStats'
          maybeBodyweight =
            if isBodyweight liftStats' 
                then bodyweight
                else 0
          weight' = pr' * percent / 100 - maybeBodyweight 
          roundedWeight = roundToMultiple weight' multiple


roundToMultiple :: Float -> Float -> Float
roundToMultiple n multiple = multiple * multiples
    where multiples = fromIntegral $ round (n / multiple) :: Float


logSetType :: Program.SetType -> Log.SetType
logSetType Program.PR = Log.PR True
logSetType _          = Log.Work


statsOfLift :: Stats -> Lift -> LiftStats
statsOfLift stats lift' = 
    head
    $ filter (\ls -> lift ls == lift')
    $ lifts stats


cycleOfLift :: [Program.LiftCycle] -> Lift -> Program.LiftCycle
cycleOfLift cycles lift =
    head
    $ filter (\cycle -> Program.lift cycle == lift) cycles 


sessionOfLiftStats :: Program.LiftCycle -> LiftStats -> [Program.Set]
sessionOfLiftStats liftCycle' stats = sessions !! position'
    where position' = position $ liftCycle stats   
          sessions = Program.prSession liftCycle' : (cycle $ Program.workSessionCycle liftCycle')
