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
        , Log.sets = map (logSet pr) session
        }
    where pr = undefined
          session = undefined


liftInPosition :: Program.LiftGroupCycle
               -> CyclePosition
               -> Lift
liftInPosition cycle cyclePosition =  
    cycle !! (position cyclePosition)

logSet :: Weight
       -> Program.Set
       -> Log.Set
logSet pr (Program.Set reps percent setType) =  
    Log.Set reps weight (logSetType setType)
    where weight = pr * percent / 100 

logSetType :: Program.SetType -> Log.SetType
logSetType Program.PR = Log.PR True
logSetType _          = Log.Work

