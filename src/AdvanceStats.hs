module AdvanceStats where

import EndgameStats
import EndgameLog

advanceStats :: Stats -> Log -> Stats
advanceStats stats log = stats
    { liftGroupPositions = 
        map advanceCyclePosition 
        $ liftGroupPositions stats
    , lifts = 
        map (uncurry $ advanceLift)
        $ zipLiftSessionAndStats
        (liftSessions log)
        (lifts stats) }


advanceCyclePosition :: CyclePosition -> CyclePosition
advanceCyclePosition (CyclePosition pos len) = CyclePosition
    { position = (pos + 1) `mod` len
    , EndgameStats.length = len }


zipLiftSessionAndStats :: [LiftSession] -> [LiftStats] -> [(LiftSession, LiftStats)]
zipLiftSessionAndStats = undefined


advanceLift :: LiftSession -> LiftStats -> LiftStats
advanceLift session stats = undefined