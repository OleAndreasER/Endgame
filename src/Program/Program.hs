{-# LANGUAGE DeriveGeneric #-}

module Program.Program
    ( Program
    , liftGroupCycles
    , program
    , lift
    , liftCycle
    , progression
    , isBodyweight
    , prSession
    , liftList
    , hasLift
    , setProgression
    , toggleBodyweight
    ) where

import Types.General
    ( Lift
    , Weight
    )
import Program.LiftGroupCycle
    ( LiftGroupCycle )
import Program.LiftCycle
    ( LiftCycle )
import qualified Program.LiftCycle as LiftCycle
    ( prSession )
import Program.Session
    ( Session )
import qualified Program.LiftInfo as LiftInfo
    ( LiftInfo
        (..)
    )
import qualified Data.Map as Map
import Data.Binary
import GHC.Generics
    (Generic)

data Program = Program
    { liftGroupCycles :: [LiftGroupCycle]
    , liftCycleMap :: Map.Map Lift LiftCycle
    , progressionMap :: Map.Map Lift Weight
    , isBodyweightMap :: Map.Map Lift Bool
    , liftsInOrder :: [Lift]
    } deriving (Show, Read, Eq, Generic)

instance Binary Program

program :: [LiftGroupCycle]
        -> [(LiftInfo.LiftInfo, LiftCycle)]
        -> Program
program liftGroupCycles liftInfos = Program
    liftGroupCycles
    (Map.fromList $ liftCycle <$> liftInfos)
    (Map.fromList $ progression <$> liftInfos)
    (Map.fromList $ isBodyweight <$> liftInfos)
    (LiftInfo.name . fst <$> liftInfos)
  where
    liftCycle (info, cycle) =
        ( LiftInfo.name info, cycle )
    progression (info, _) =
        ( LiftInfo.name info, LiftInfo.progression info )
    isBodyweight (info, _) =
        ( LiftInfo.name info, LiftInfo.isBodyweight info )

lift :: LiftInfo.LiftInfo -> LiftCycle -> (LiftInfo.LiftInfo, LiftCycle)
lift = (,)

liftCycle :: Lift -> Program -> Maybe LiftCycle
liftCycle lift program = Map.lookup lift $ liftCycleMap program

progression :: Lift -> Program -> Maybe Weight
progression lift program = Map.lookup lift $ progressionMap program

isBodyweight :: Lift -> Program -> Maybe Bool
isBodyweight lift program = Map.lookup lift $ isBodyweightMap program

prSession :: Lift -> Program -> Maybe Session
prSession lift program = LiftCycle.prSession <$> liftCycle lift program

liftList :: Program -> [Lift]
liftList = liftsInOrder

hasLift :: Lift -> Program -> Bool
hasLift lift program = lift `elem` liftsInOrder program

toProgression :: (Weight -> Weight) -> Lift -> Program -> Program
toProgression f lift program = program
    { progressionMap = Map.adjust f lift $ progressionMap program }

setProgression :: Lift -> Weight -> Program -> Program
setProgression lift weight = toProgression (\_ -> weight) lift

toIsBodyweight :: (Bool -> Bool) -> Lift -> Program -> Program
toIsBodyweight f lift program = program
    { isBodyweightMap = Map.adjust f lift $ isBodyweightMap program }

toggleBodyweight :: Lift -> Program -> Program
toggleBodyweight = toIsBodyweight not
