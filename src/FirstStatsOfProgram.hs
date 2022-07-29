module FirstStatsOfProgram (firstStatsOfProgram) where

import Types.Stats as Stats
import Types.Program as Program


firstStatsOfProgram :: Program -> Stats
firstStatsOfProgram (Program liftGroupCycles liftCycles) = Stats
    { liftGroupPositions =
        map liftGroupPosition liftGroupCycles 
    , lifts = map liftStats liftCycles
    , bodyweight = 0
    }

liftGroupPosition :: LiftGroupCycle -> CyclePosition
liftGroupPosition liftGroupCycle =
    endingCycle $ Prelude.length liftGroupCycle

liftStats :: LiftCycle -> LiftStats
liftStats liftCycle = LiftStats
    { Stats.lift = Program.lift liftCycle
    , progression = 1.25
    , pr = 0 
    , liftCycle = CyclePosition 0 1 
    , isBodyweight = False
    }
