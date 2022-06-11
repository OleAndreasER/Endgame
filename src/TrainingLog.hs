{-# LANGUAGE DeriveGeneric #-}

module TrainingLog where 
import GHC.Generics (Generic)
import Data.Binary

data SetType = Work | PR Bool
    deriving (Generic, Show)
    
instance Binary SetType


data Set = Set {
    reps :: Integer,
    weight :: Float,
    setType :: SetType
} deriving (Generic, Show)

instance Binary Set


data LiftSession = LiftSession {
    lift :: String,
    sets :: [Set],
    sessionType :: SetType
}   deriving (Generic, Show)

instance Binary LiftSession


data Log = Log {
    date :: String,
    liftSessions :: [LiftSession]
}   deriving (Generic, Show)

instance Binary Log



testLog = Log {
    date = "05/05/2022",
    liftSessions = [
        LiftSession {
            lift = "Bench",
            sets = [
                Set {
                    reps = 3,
                    weight = 100,
                    setType = PR True
                }
            ],
            sessionType = PR True
        },
        LiftSession {
            lift = "Squat",
            sets = [
                Set {
                    reps = 3,
                    weight = 200,
                    setType = PR False
                }
            ],
            sessionType = PR False
        }
    ]
}