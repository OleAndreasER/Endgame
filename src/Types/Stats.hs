{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types.Stats where

import Types.General (Lift, Weight)
import Data.Binary
import GHC.Generics (Generic)

type Bodyweight = Float


instance Binary Stats
instance Binary CyclePosition
instance Binary LiftStats


data CyclePosition = CyclePosition
    { position :: Int
    , length :: Int
    } deriving (Generic, Eq, Show, Read)


data Stats = Stats 
    { liftGroupPositions :: [CyclePosition]
    , lifts :: [LiftStats]
    , bodyweight :: Bodyweight
    } deriving (Generic, Show, Eq, Read)


data LiftStats = LiftStats
    { lift :: Lift
    , progression :: Weight --Progression increment
    , pr :: Weight
    , liftCycle :: CyclePosition
    , isBodyweight :: Bool
    } deriving (Generic, Show, Eq, Read)


addWork :: Int -> Lift -> Stats -> Stats
addWork work = toLiftStats $ addWorkToLift work

addWorkToLift :: Int -> LiftStats -> LiftStats
addWorkToLift work liftStats = liftStats
    { liftCycle = addLength work $ liftCycle liftStats }

addLength :: Int -> CyclePosition -> CyclePosition
addLength n (CyclePosition pos len) = CyclePosition pos (len+n)

setPR :: Weight -> LiftStats -> LiftStats
setPR newPr liftStats = liftStats
    { pr = newPr }

addProgressions :: Int -> LiftStats -> LiftStats
addProgressions n liftStats = liftStats
    { pr =
        pr liftStats 
        + (fromIntegral n * progression liftStats)
    }

setProgression :: Weight -> LiftStats -> LiftStats
setProgression newProgression liftStats = liftStats
    { progression = newProgression }

setCycle :: Int -> Int -> LiftStats -> LiftStats
setCycle pos len liftStats = liftStats
    { liftCycle = CyclePosition pos len }

toggleBodyweight :: LiftStats -> LiftStats
toggleBodyweight liftStats = liftStats
    { isBodyweight = not $ isBodyweight liftStats }

toLiftStats :: (LiftStats -> LiftStats) -> Lift -> Stats -> Stats
toLiftStats f lift' stats =
    stats { lifts = map maybeF $ lifts stats}
  where
    maybeF liftStats
        | lift liftStats == lift' = f liftStats
        | otherwise               = liftStats

liftIsInStats :: Lift -> Stats -> Bool
liftIsInStats lift' stats =
    any ((== lift') . lift)
    $ lifts stats

statsOfLift :: Stats -> Lift -> LiftStats
statsOfLift stats lift' = head
    $ filter ((lift' ==) . lift)
    $ lifts stats    
