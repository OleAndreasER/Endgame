module FirstStatsOfProgram where

import Types.EndgameStats
import Types.EndgameProgram

firstStatsOfProgram :: Program -> Stats
firstStatsOfProgram (Program liftGroupCycles liftCycles) = Stats
    { liftGroupPositions =
        map liftGroupPosition liftGroupCycles 
    , lifts = undefined
    , bodyweight = 0 }

liftGroupPosition :: LiftGroupCycle -> CyclePosition
liftGroupPosition liftGroupCycle =
    CyclePosition 0 $ Prelude.length liftGroupCycle