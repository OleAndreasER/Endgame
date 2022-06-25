module NewLog where 

import qualified EndgameLog as Log
import qualified EndgameProgram as Program
import EndgameGeneralTypes (Lift)
import EndgameStats 


log :: String
    -> Program.Program
    -> Stats
    -> Log.Log

log date (Program.Program liftGroupCycles liftCycles) stats = 
    Log.Log 
        { Log.date = date
        , Log.liftSessions = 
            map (\lift -> Log.LiftSession
                { Log.lift = lift
                , Log.sets = undefined 
                })
            $ map (uncurry $ liftInPosition)
            $ zip (liftGroupCycles)
            $ liftGroupPositions stats
        }


liftInPosition :: Program.LiftGroupCycle
               -> CyclePosition
               -> Lift
liftInPosition cycle cyclePosition =  
    cycle !! (position cyclePosition)
