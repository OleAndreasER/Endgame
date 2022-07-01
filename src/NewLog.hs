module NewLog where 

import qualified EndgameLog as Log
import qualified EndgameProgram as Program
import EndgameGeneralTypes (Lift, Weight)
import EndgameStats 


log :: String
    -> Program.Program
    -> Stats
    -> Log.Log
log date (Program.Program liftGroupCycles liftCycles) stats = 
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
        , Log.sets = map (logSet liftStats') session
        }
    where liftStats' = statsOfLift stats lift
          session = sessionOfLiftStats (cycleOfLift liftCycles lift)
                                       (statsOfLift stats lift)


liftInPosition :: Program.LiftGroupCycle
               -> CyclePosition
               -> Lift
liftInPosition cycle cyclePosition =  
    cycle !! (position cyclePosition)


logSet :: LiftStats -> Program.Set -> Log.Set
logSet liftStats' (Program.Set reps percent setType) =  
    Log.Set reps roundedWeight (logSetType setType)
    where pr' = pr liftStats'
          multiple = progression liftStats'
          weight' = pr' * percent / 100 
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
