module AdvanceStats where

import EndgameStats
import EndgameLog

advanceStats :: Stats -> Log -> Stats
advanceStats stats log = stats
    { liftGroupPositions = 
        map advanceCyclePosition 
        $ liftGroupPositions stats
    , lifts = undefined } 

advanceCyclePosition :: CyclePosition -> CyclePosition
advanceCyclePosition (CyclePosition pos len) =
    CyclePosition
        { position = (pos + 1) `mod` len 
        , EndgameStats.length = len }