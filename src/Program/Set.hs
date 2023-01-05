{-# LANGUAGE DeriveGeneric #-}

module Program.Set
    ( Set (..)
    , SetType (..)
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
