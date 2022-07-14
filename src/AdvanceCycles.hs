{-# LANGUAGE NamedFieldPuns #-}
module AdvanceCycles where

import Types.Stats
import qualified Types.Stats as Stats (length)
import Types.Log

advanceCycles :: Log -> Stats -> Stats
advanceCycles log = advanceLiftGroups . (advanceLifts log)

advanceLifts :: Log -> Stats -> Stats
advanceLifts log stats = 
    foldr (toLiftStats advanceLiftStats) stats 
    $ doneLifts log

advanceLiftStats :: LiftStats -> LiftStats
advanceLiftStats liftStats@(LiftStats { liftCycle }) =
    liftStats { liftCycle = advanceCyclePosition liftCycle }

advanceLiftGroups :: Stats -> Stats
advanceLiftGroups stats@(Stats { liftGroupPositions }) = stats
    { liftGroupPositions = map advanceCyclePosition $ liftGroupPositions }

advanceCyclePosition :: CyclePosition -> CyclePosition
advanceCyclePosition cyclePosition@(CyclePosition { position, Stats.length }) = 
    cyclePosition { position = (position + 1) `mod` length }