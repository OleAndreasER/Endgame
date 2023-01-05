{-# LANGUAGE DeriveGeneric #-}

module Program.Set
    ( Set
        ( lift
        , reps
        , percent
        , setType
        )
    , SetType (..)
    , prSet
    , workSets
    ) where

import GHC.Generics (Generic)
import Types.General
    ( Lift
    , Reps
    , Percent
    )

data Set = Set
    { lift :: Lift
    , reps :: Reps
    , percent :: Percent
    , setType :: SetType
    } deriving (Show, Read, Eq, Generic)

data SetType = PR | Work
    deriving (Show, Read, Eq, Generic)

prSet :: Lift -> Reps -> Set
prSet lift reps = Set lift reps 100 PR

workSets :: Lift -> Int -> Reps -> Percent -> [Set]
workSets _ 0 _ _ = []
workSets lift sets reps percent =
    Set lift reps percent Work : workSets lift (sets-1) reps percent
