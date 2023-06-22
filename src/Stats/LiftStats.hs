{-# LANGUAGE DeriveGeneric #-}

module Stats.LiftStats
    ( LiftStats
    , pr
    , cyclePosition
    , cycleLength
    , newLiftStats
    , addWork
    , setPr
    , increasePr
    , setCycle
    , advanceCycle
    ) where

import Data.Binary
import GHC.Generics
    ( Generic )
import Types.General
    ( Weight )
import Data.Aeson (FromJSON, ToJSON)

data LiftStats = LiftStats
    { pr :: Weight
    , cyclePosition :: Int
    , cycleLength :: Int
    } deriving (Generic, Show, Eq, Read)

instance Binary LiftStats
instance ToJSON LiftStats
instance FromJSON LiftStats

newLiftStats :: LiftStats
newLiftStats = LiftStats 0 0 1

addWork :: Int -> LiftStats -> LiftStats
addWork work liftStats = liftStats
    { cycleLength = cycleLength liftStats + work }

setPr :: Weight -> LiftStats -> LiftStats
setPr newPr liftStats = liftStats
    { pr = newPr }

increasePr :: Weight -> LiftStats -> LiftStats
increasePr increase liftStats =
    setPr (pr liftStats + increase) liftStats

setCycle :: Int -> Int -> LiftStats -> LiftStats
setCycle pos len liftStats = liftStats 
    { cyclePosition = pos
    , cycleLength = len
    }

advanceCycle :: LiftStats -> LiftStats
advanceCycle liftStats = setCycle
    ((cyclePosition liftStats + 1) `mod` cycleLength liftStats)
    (cycleLength liftStats)
    liftStats
