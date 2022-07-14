{-# LANGUAGE DeriveGeneric
           , NamedFieldPuns #-}

module Types.Log where 

import GHC.Generics (Generic)
import Data.Binary
import Types.GeneralTypes (Lift, Reps, Weight)

instance Binary SetType
instance Binary Set
instance Binary LiftSession
instance Binary Log


data SetType
    = Work 
    | PR Bool
    deriving (Generic, Show, Read, Eq)


data Set = Set 
    { reps :: Reps
    , weight :: Weight
    , setType :: SetType 
    } deriving (Generic, Show, Read, Eq)


data LiftSession = LiftSession 
    { lift :: Lift
    , sets :: [Set]
    } deriving (Generic, Show, Read, Eq)


data Log = Log 
    { label :: String
    , liftSessions :: [LiftSession]
    } deriving (Generic, Show, Read, Eq)


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


did :: Lift -> Log -> Bool
did target log =
    any (\session -> lift session == target)
    $ liftSessions log 

failSet :: Set -> Set
failSet (Set reps weight setType) = 
    Set reps weight $ case setType of 
        Work         -> Work
        PR succeeded -> PR (not succeeded)
    
failSession :: LiftSession -> LiftSession
failSession session = 
    let sets' = sets session 
    in session { sets = (failSet $ head sets') : tail sets' }

failLift :: Lift -> Log -> Log
failLift = toLift failSession

toLift :: (LiftSession -> LiftSession) -> Lift -> Log -> Log
toLift f lift' log = 
    let maybeF session | lift session == lift' = f session
                       | otherwise             = session
    in log { liftSessions = map maybeF $ liftSessions log }

doneLifts :: Log -> [Lift]
doneLifts (Log { liftSessions }) = map lift liftSessions