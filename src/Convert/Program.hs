module Convert.Program 
    ( convert
    ) where

import Types.General
    ( Lift )
import qualified Types.Program as Old
import qualified Types.Stats as Old.Stats
import qualified Program.Program as New
import qualified Program.LiftCycle as New
import qualified Program.Set as New.Set
import Program.LiftInfo
    ( LiftInfo
        (..)
    )

convert :: Old.Stats.Stats -> Old.Program -> New.Program
convert stats old = New.program
    (Old.liftGroupCycles old)
    (convertLiftCycle stats <$> Old.liftCycles old)

convertLiftCycle :: Old.Stats.Stats 
                 -> Old.LiftCycle
                 -> (LiftInfo, New.LiftCycle)
convertLiftCycle stats oldCycle = New.lift
    (LiftInfo
        { name = Old.lift oldCycle
        , progression = Old.Stats.progression liftStats'
        , isBodyweight = Old.Stats.isBodyweight liftStats'
        }) $
    (map . map) (convertSet $ Old.lift oldCycle) $
    take 2 $ Old.sessions oldCycle -- Assumes there are only 2 different sessions
  where
    liftStats' :: Old.Stats.LiftStats
    liftStats' = Old.Stats.statsOfLift stats $ Old.lift oldCycle

convertSet :: Lift -> Old.Set -> New.Set.Set
convertSet lift (Old.Set reps _ Old.PR) =
    New.Set.prSet lift reps

convertSet lift (Old.Set reps percent Old.Work) =
    head $ New.Set.workSets lift 1 reps percent
