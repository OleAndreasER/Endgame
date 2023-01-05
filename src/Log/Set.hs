{-# LANGUAGE DeriveGeneric #-}

module Log.Set
    ( Set (..)
    , SetType (..)
    , failSet
    ) where

import GHC.Generics
    ( Generic
    )
import Data.Binary
import Types.General
    ( Reps
    , Weight
    , Lift
    )

data SetType
    = Work 
    | PR Bool
    deriving (Generic, Show, Read, Eq)

instance Binary SetType

data Set = Set 
    { lift :: Lift
    , reps :: Reps
    , weight :: Weight
    , setType :: SetType
    } deriving (Generic, Show, Read, Eq)

instance Binary Set

failSet :: Set -> Set
failSet (Set lift reps weight setType) = 
    Set lift reps weight $ case setType of 
        Work         -> Work
        PR succeeded -> PR (not succeeded)
    