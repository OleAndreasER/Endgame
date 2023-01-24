{-# LANGUAGE DeriveGeneric #-}

module Stats.LiftStats
    ( LiftStats
    , pr
    , cyclePosition
    , cycleLength
    , newLiftStats
    , addWork
    , setPr
    , setCycle
    ) where

import Data.Binary
import GHC.Generics
    ( Generic )
import Types.General
    ( Weight )

data LiftStats = LiftStats
    { pr :: Weight
    , cyclePosition :: Int
    , cycleLength :: Int
    } deriving (Generic, Show, Eq, Read)

instance Binary LiftStats

newLiftStats :: LiftStats
newLiftStats = LiftStats 0 0 1

addWork :: Int -> LiftStats -> LiftStats
addWork work liftStats = liftStats
    { cycleLength = cycleLength liftStats + work }

setPr :: Weight -> LiftStats -> LiftStats
setPr newPr liftStats = liftStats
    { pr = newPr }

setCycle :: Int -> Int -> LiftStats -> LiftStats
setCycle pos len liftStats = liftStats 
    { cyclePosition = pos
    , cycleLength = len
    }
