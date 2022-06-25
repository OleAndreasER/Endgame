{-# LANGUAGE DeriveGeneric #-}

module EndgameLog where 

import GHC.Generics (Generic)
import Data.Binary
import EndgameGeneralTypes (Lift, Reps, Weight)

data SetType
    = Work 
    | PR Bool
    deriving (Generic, Show, Eq)
    
instance Binary SetType


data Set = Set Reps Weight SetType
    deriving (Generic, Show, Eq)

instance Binary Set


data LiftSession = LiftSession 
    { lift :: Lift
    , sets :: [Set]
    } deriving (Generic, Show)

instance Binary LiftSession


data Log = Log 
    { date :: String
    , liftSessions :: [LiftSession]
    } deriving (Generic, Show)

instance Binary Log

testLog = Log 
    "06/05/2022"
    [LiftSession "Bench" 
        [Set 3 100 (PR True),
         Set 5 87 Work],
     LiftSession "Squat"
        [Set 3 200 (PR False),
         Set 5 170 Work],
     LiftSession "Row"
        [Set 5 75 Work,
         Set 5 75 Work,
         Set 5 75 Work]]
