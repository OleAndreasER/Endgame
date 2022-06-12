{-# LANGUAGE DeriveGeneric #-}

module EndgameStats where

import EndgameGeneralTypes (Lift, Weight)
import Data.Binary
import GHC.Generics (Generic)

type Bodyweight = Float

data Stats = Stats [LiftStats] Bodyweight
    deriving (Generic)

instance Binary Stats


data LiftStats = LiftStats Lift Weight 
    deriving (Generic)

instance Binary LiftStats
    