{-# LANGUAGE DeriveGeneric #-}

module EndgameProgram where
import GHC.Generics (Generic)
import Data.Binary

data Program = Program [LiftGroupCycle] [LiftCycle]
    deriving (Show, Generic)

instance Binary Program


data SetType = PR | Work
    deriving (Show, Enum, Generic)

instance Binary SetType


data Set = Set {
    reps :: Integer,
    percent :: Float,
    setType :: SetType
}   deriving (Show, Generic)

instance Binary Set


--[Squat, Deadlift]
data LiftGroupCycle = LiftGroupCycle [String]
    deriving (Show, Generic)

instance Binary LiftGroupCycle


data LiftCycle = LiftCycle {
    lift :: String,
    prSession :: [Set],
    workSessionCycle :: [[Set]] -- [[]] because you might want more kinds of work sessions.
}   deriving (Show, Generic)

instance Binary LiftCycle

everyotherday :: Program
everyotherday = Program 
    [LiftGroupCycle ["Press", "Bench"],
     LiftGroupCycle ["Squat", "Deadlift"],
     LiftGroupCycle ["Chin", "Row"]]
     
    [LiftCycle {lift = "Press",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Bench",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Squat",
                prSession = [Set 3 100 PR, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Deadlift",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Chin",
                prSession = [Set 3 100 PR, Set 5 87 Work, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Row",
                prSession = [Set 3 100 PR, Set 5 87 Work, Set 5 87 Work],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]}]
