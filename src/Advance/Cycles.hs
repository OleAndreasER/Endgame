{-# LANGUAGE NamedFieldPuns #-}
module Advance.Cycles (advanceCycles) where

import Types.Stats as Stats
import Types.Program as Program
import qualified CurrentLog as CurrentLog (lifts)

{-After a log, 
  all the liftGroupCycles advance e.g. 
  Press -> Bench, 
  and the cycles of each lift present in the log advances e.g.
  Press: PR -> Work.
-}

advanceCycles :: Program -> Stats -> Stats
advanceCycles program = advanceLiftGroups . advanceLifts program

advanceLifts :: Program -> Stats -> Stats
advanceLifts program stats = 
    foldr (toLiftStats advanceLiftStats) stats 
    $ CurrentLog.lifts program stats

advanceLiftStats :: LiftStats -> LiftStats
advanceLiftStats liftStats@(LiftStats { liftCycle }) = liftStats
    { liftCycle = advanceCyclePosition liftCycle }

advanceLiftGroups :: Stats -> Stats
advanceLiftGroups stats@(Stats { liftGroupPositions }) = stats
    { liftGroupPositions =
        map advanceCyclePosition liftGroupPositions
    }

advanceCyclePosition :: CyclePosition -> CyclePosition
advanceCyclePosition
    cyclePosition@(CyclePosition { position, Stats.length })
  = cyclePosition { position = (position + 1) `mod` length }
