module TrainingLog where 

data SetType = Work | PR Bool
    deriving (Show)

data Set = Set {
    reps :: Integer,
    weight :: Float,
    setType :: SetType
}

data LiftSession = LiftSession {
    lift :: String,
    sets :: [Set],
    sessionType :: SetType
}

data Log = Log {
    date :: String,
    liftSessions :: [LiftSession]
}
