{-# LANGUAGE DeriveGeneric #-}

module EndgameStats where

import EndgameGeneralTypes (Lift, Weight)
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
        [ CyclePosition 0 2
        , CyclePosition 1 3
        , CyclePosition 1 2 ]
    , lifts =
        [ LiftStats "Press" 1.25 57.5 (CyclePosition 2 3) False
        , LiftStats "Bench" 1.25 96.25 (CyclePosition 2 5) False
        , LiftStats "Squat" 2.5 145 (CyclePosition 1 5) False
        , LiftStats "Deadlift" 2.5 152.5 (CyclePosition 3 5) False
        , LiftStats "Chin" 1.25 105 (CyclePosition 4 5) True
        , LiftStats "Row" 1.25 91.25 (CyclePosition 0 5) False ]
    , bodyweight = 72.7
    }