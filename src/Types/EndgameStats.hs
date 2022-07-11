{-# LANGUAGE DeriveGeneric #-}

module Types.EndgameStats where

import Types.EndgameGeneralTypes (Lift, Weight)
import Data.Binary
import GHC.Generics (Generic)

type Bodyweight = Float


data CyclePosition = CyclePosition
    { position :: Int
    , length :: Int
    } deriving (Generic, Eq, Show, Read)

instance Binary CyclePosition


data Stats = Stats 
    { liftGroupPositions :: [CyclePosition]
    , lifts :: [LiftStats]
    , bodyweight :: Bodyweight
    } deriving (Generic, Show, Eq, Read)

instance Binary Stats


data LiftStats = LiftStats
    { lift :: Lift
    , progression :: Weight --Progression increment
    , pr :: Weight
    , liftCycle :: CyclePosition
    , isBodyweight :: Bool
    } deriving (Generic, Show, Eq, Read)

instance Binary LiftStats

testStats :: Stats
testStats = Stats
    { liftGroupPositions = 
        [ CyclePosition 0 3
        , CyclePosition 0 3
        , CyclePosition 0 2 ]
    , lifts =
        [ LiftStats "Press" 1.25 57.5 (CyclePosition 0 4) False
        , LiftStats "Bench" 1.25 96.25 (CyclePosition 0 4) False
        , LiftStats "Squat" 2.5 145 (CyclePosition 0 4) False
        , LiftStats "Deadlift" 2.5 150 (CyclePosition 0 4) False
        , LiftStats "Chin" 1.25 105 (CyclePosition 0 4) True
        , LiftStats "Row" 1.25 91.25 (CyclePosition 0 3) False ]
    , bodyweight = 72.7
    }

addWork :: Lift -> Stats -> Stats
addWork = toLiftStats addWorkToLift

addWorkToLift :: LiftStats -> LiftStats
addWorkToLift liftStats = liftStats
    { liftCycle = increment $ liftCycle liftStats }

increment :: CyclePosition -> CyclePosition
increment (CyclePosition pos len) = CyclePosition (pos+1) (len+1)

toLiftStats :: (LiftStats -> LiftStats) -> Lift -> Stats -> Stats
toLiftStats f lift' stats =
    let maybeF liftStats | lift liftStats == lift' = f liftStats
                         | otherwise               = liftStats
    in stats { lifts = map maybeF $ lifts stats}