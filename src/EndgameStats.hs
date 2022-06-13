{-# LANGUAGE DeriveGeneric #-}

module EndgameStats where

import EndgameGeneralTypes (Lift, Weight)
import Data.Binary
import GHC.Generics (Generic)

type Bodyweight = Float

data Stats = Stats [LiftStats] Bodyweight
    deriving (Generic)

instance Binary Stats


data CyclePosition = CyclePosition Integer Integer 
    deriving (Generic)

instance Binary CyclePosition

type LiftCyclePosition = CyclePosition
type LiftGroupCyclePosition = CyclePosition


data LiftStats = LiftStats Lift Weight LiftCyclePosition
    deriving (Generic)

instance Binary LiftStats
    