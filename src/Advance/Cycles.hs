{-# LANGUAGE NamedFieldPuns #-}
module Advance.Cycles (advanceCycles, regressCycles) where

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
advanceCycles = advanceCycles' 1

regressCycles :: Program -> Stats -> Stats
regressCycles = advanceCycles' (-1)

advanceCycles' :: Int -> Program -> Stats -> Stats
advanceCycles' n program =
    advanceLiftGroups n
    . advanceLifts n program

advanceLifts :: Int -> Program -> Stats -> Stats
advanceLifts n program stats = 
    foldr (toLiftStats (advanceLiftStats n)) stats 
    $ CurrentLog.lifts program stats

advanceLiftStats :: Int -> LiftStats -> LiftStats
advanceLiftStats n liftStats@(LiftStats { liftCycle }) = liftStats
    { liftCycle = advanceCyclePosition n liftCycle }

advanceLiftGroups :: Int -> Stats -> Stats
advanceLiftGroups n stats@(Stats { liftGroupPositions }) = stats
    { liftGroupPositions =
        map (advanceCyclePosition n) liftGroupPositions
    }

advanceCyclePosition :: Int -> CyclePosition -> CyclePosition
advanceCyclePosition
    n
    cyclePosition@(CyclePosition { position, Stats.length })
  = cyclePosition { position = (position + n) `mod` length }
