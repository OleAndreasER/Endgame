{-# LANGUAGE DeriveGeneric #-}

module Types.Program where
import GHC.Generics (Generic)
import Data.Binary
import Types.General (Reps, Percent, Lift)

data Program = Program [LiftGroupCycle] [LiftCycle]
    deriving (Show, Generic)

instance Binary Program


data SetType = PR | Work
    deriving (Show, Eq, Generic)

instance Binary SetType


data Set = Set Reps Percent SetType
    deriving (Show, Eq, Generic)

instance Binary Set


type LiftGroupCycle = [Lift]
   
data LiftCycle = LiftCycle 
    { lift :: Lift
    , prSession :: [Set]
    , workSessionCycle :: [[Set]]
    } deriving (Show, Generic)

instance Binary LiftCycle

