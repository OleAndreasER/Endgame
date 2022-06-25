module EndgameGeneralTypes where


type Reps = Integer
type Percent = Float
type Lift = String
type Weight = Float

data CyclePosition = CyclePosition
    { position :: Int
    , length :: Int
    } deriving (Generic, Eq, Show, Read)

instance Binary CyclePosition