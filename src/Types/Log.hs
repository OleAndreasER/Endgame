{-# LANGUAGE DeriveGeneric
           , NamedFieldPuns #-}

module Types.Log where 

import GHC.Generics (Generic)
import Data.Binary
import Types.General (Lift, Reps, Weight)
import Data.List (find)
import Control.Monad ((<=<))
import Control.Applicative.Tools ((<.>))

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
failSession session = session
    { sets = (failSet $ head sets') : tail sets' }
  where
    sets' = sets session 

failLift :: Lift -> Log -> Log
failLift = toLift failSession

toLift :: (LiftSession -> LiftSession) -> Lift -> Log -> Log
toLift f lift' log = log
    { liftSessions = map maybeF $ liftSessions log }
  where
    maybeF :: LiftSession -> LiftSession
    maybeF session
        | lift session == lift' = f session
        | otherwise             = session

doneLifts :: Log -> [Lift]
doneLifts (Log { liftSessions }) = map lift liftSessions

hasPR :: LiftSession -> Bool
hasPR liftSession = 
    any ((== PR True) . setType)
    $ sets liftSession 

liftSetType :: Lift -> Log -> Maybe SetType
liftSetType lift = setType
    <.> firstSet
    <=< sessionOfLift lift

sessionOfLift :: Lift -> Log -> Maybe LiftSession
sessionOfLift lift' log =
    find ((lift' ==) . lift) $ liftSessions log 

firstSet :: LiftSession -> Maybe Set
firstSet (LiftSession _ [])   = Nothing
firstSet (LiftSession _ sets) = Just $ head sets
