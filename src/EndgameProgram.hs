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

exProgram = Program [LiftTypeCycle ["Press", "Bench"],
                     LiftTypeCycle ["Squat", "Deadlift"],
                     LiftTypeCycle ["Chin", "Row"]]
                    [LiftCycle "Press"    [LiftSession PR [Set 3 100 PR, Set 5 87 Work],
                                           LiftSession Work [Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]],
                     LiftCycle "Bench"    [LiftSession PR [Set 3 100 PR, Set 5 87 Work],
                                           LiftSession Work [Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]],   
                     LiftCycle "Squat"    [LiftSession PR [Set 3 100 PR, Set 5 87 Work],
                                           LiftSession Work [Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]],   
                     LiftCycle "Deadlift" [LiftSession PR [Set 3 100 PR],
                                           LiftSession Work [Set 5 87 Work, Set 5 87 Work]],
                     LiftCycle "Chin"     [LiftSession PR [Set 3 100 PR, Set 5 87 Work, Set 5 87 Work],
                                           LiftSession Work [Set 5 87 Work, Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]],
                     LiftCycle "Row"      [LiftSession PR [Set 3 100 PR, Set 5 87 Work, Set 5 87 Work],
                                           LiftSession Work [Set 5 87 Work, Set 5 87 Work, Set 5 87 Work, Set 5 87 Work]]]