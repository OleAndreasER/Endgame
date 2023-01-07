{-# LANGUAGE DeriveGeneric #-}

module Log.Set
    ( Set
        (..)
    , SetType
        (..)
    , prSet
    , workSets
    , fail
    ) where

import GHC.Generics
    ( Generic )
import Data.Binary
import Types.General
    ( Reps
    , Weight
    , Lift
    )
import Prelude hiding
    ( fail )

data SetType = Work | PR Bool
    deriving (Generic, Show, Read, Eq)

instance Binary SetType

data Set = Set 
    { lift :: Lift
    , reps :: Reps
    , weight :: Weight
    , setType :: SetType
    } deriving (Generic, Show, Read, Eq)

instance Binary Set

prSet :: Lift -> Reps -> Weight -> Set
prSet lift reps weight = Set lift reps weight (PR True)

workSets :: Lift -> Int -> Reps -> Weight -> [Set]
workSets lift sets reps weight =
    take sets $ repeat $ Set lift reps weight Work

fail :: Set -> Set
fail (Set lift reps weight setType) = 
    Set lift reps weight $ case setType of 
        Work         -> Work
        PR succeeded -> PR (not succeeded)
    