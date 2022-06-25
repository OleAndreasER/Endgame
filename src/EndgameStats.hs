{-# LANGUAGE DeriveGeneric #-}

module EndgameStats where

import EndgameGeneralTypes (Lift, Weight, CyclePosition)
import Data.Binary
import GHC.Generics (Generic)

type Bodyweight = Float

data Stats = Stats 
    { lifts :: [LiftStats]
    , bodyweight :: Bodyweight
    } deriving (Generic, Show, Eq, Read)

instance Binary Stats


data LiftStats = LiftStats
    { lift :: Lift
    , pr :: Weight
    , cyclePosition :: CyclePosition
    } deriving (Generic, Show, Eq, Read)

instance Binary LiftStats
    