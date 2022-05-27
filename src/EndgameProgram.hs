module EndgameProgram where

data Program = Program [Cycle] [LiftCycle]

data Set = Set Integer Integer Float --Sets, Reps, Percent
    deriving (Show)

data LiftCycle = LiftCycle String [[Set]]
    deriving (Show)

--Example: Squat -> Deadlift ->
data Cycle = Cycle [String]
    deriving (Show)