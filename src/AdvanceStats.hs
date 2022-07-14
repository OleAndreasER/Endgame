module AdvanceStats (advanceStats) where

import qualified Types.Stats as Stats (length, lift)
import Types.Stats
import qualified Types.Log as Log (lift)
import Types.Log
import Types.General (Weight)

advanceStats :: Stats -> Log -> Stats
advanceStats stats log = stats
    { liftGroupPositions = 
        map advanceCyclePosition 
        $ liftGroupPositions stats
    , lifts = 
        (++) (otherLifts (liftSessions log) (lifts stats))
        $ map (uncurry $ advanceLift)
        $ liftSessionStatsPairs (liftSessions log) (lifts stats) }


otherLifts :: [LiftSession] -> [LiftStats] -> [LiftStats]
otherLifts sessions stats = 
    filter (\stat -> not $ any
           (\session -> Stats.lift stat == Log.lift session) sessions)
    stats


advanceCyclePosition :: CyclePosition -> CyclePosition
advanceCyclePosition (CyclePosition pos len) = CyclePosition
    { position = (pos + 1) `mod` len
    , Stats.length = len }


liftSessionStatsPairs :: [LiftSession] -> [LiftStats] -> [(LiftSession, LiftStats)]
liftSessionStatsPairs sessions stats =
    [ (session, stat)
    | session <- sessions
    , stat    <- stats
    , Stats.lift stat == Log.lift session ] 


advanceLift :: LiftSession -> LiftStats -> LiftStats
advanceLift session stats = stats
    { pr = pr stats + prChange sessionResult (progression stats)
    , liftCycle = updateLiftCycle sessionResult $ liftCycle stats }
    where (Set _ _ sessionResult) = head $ sets session


prChange :: SetType -> Weight -> Weight
prChange (PR True) progression  = progression
prChange (PR False) progression = -2 * progression
prChange Work _                 = 0


updateLiftCycle :: SetType -> CyclePosition -> CyclePosition
updateLiftCycle (PR False) cyclePosition = 
    advanceCyclePosition cyclePosition 
        { Stats.length = Stats.length cyclePosition + 1 }

updateLiftCycle _ cyclePosition = advanceCyclePosition cyclePosition