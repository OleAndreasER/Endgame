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
    , liftPosition :: CyclePosition
    , liftGroupPosition :: CyclePosition
    } deriving (Generic, Show, Eq, Read)

instance Binary LiftStats
    