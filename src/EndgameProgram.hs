module EndgameProgram where

data Program = Program [LiftTypeCycle] [LiftCycle]
    deriving (Show)

data SetType = PR | Work
    deriving (Show, Enum)

data Set = Set Integer Float SetType
    deriving (Show)

data LiftSession = LiftSession SetType [Set] 
    deriving (Show)

--[Squat, Deadlift]
data LiftTypeCycle = LiftTypeCycle [String]
    deriving (Show)

--Squat cycle: [PR [(3 100 PR), (5 87 Work)],
--              Work [(5 87 Work), (5 87 Work), (5 87 Work)] Work]
data LiftCycle = LiftCycle String [LiftSession]
    deriving (Show)