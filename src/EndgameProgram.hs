{-# LANGUAGE DeriveGeneric #-}

module EndgameProgram where
import GHC.Generics (Generic)
import Data.Binary
import EndgameGeneralTypes (Reps, Percent, Lift)

data Program = Program [LiftGroupCycle] [LiftCycle]
    deriving (Show, Generic)

instance Binary Program


data SetType = PR | Work
    deriving (Show, Eq, Generic)

instance Binary SetType


data Set = Set Reps Percent SetType
    deriving (Show, Eq, Generic)

instance Binary Set



--[Squat, Deadlift]
type LiftGroupCycle = [Lift]
   
data LiftCycle = LiftCycle {
    lift :: Lift,
    prSession :: [Set],
    workSessionCycle :: [[Set]] -- [[Set]] because you might want more kinds of work sessions.
}   deriving (Show, Generic)

instance Binary LiftCycle


-- Example Programs

everyotherday :: Program
everyotherday = Program 
    [["Press", "Bench"],
     ["Squat", "Squat", "Deadlift"],
     ["Chin", "Row"]]
     
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

everyday :: Program
everyday = Program 
    [["Press", "Bench"],
     ["Squat", "Squat", "Deadlift"],
     ["Chin", "Row"]]
     
    [LiftCycle {lift = "Press",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 85 Work, Set 5 85 Work]]},
     LiftCycle {lift = "Bench",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 85 Work, Set 5 85 Work]]},
     LiftCycle {lift = "Squat",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 85 Work, Set 5 85 Work]]},
     LiftCycle {lift = "Deadlift",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 87 Work]]},
     LiftCycle {lift = "Chin",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work]]},
     LiftCycle {lift = "Row",
                prSession = [Set 3 100 PR],
                workSessionCycle = [[Set 5 87 Work, Set 5 87 Work]]}]
