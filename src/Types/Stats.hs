{-# LANGUAGE 
    DeriveGeneric,
    NamedFieldPuns #-}

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


testStats :: Stats
testStats = Stats
    { liftGroupPositions = 
        [ CyclePosition 0 3
        , CyclePosition 0 3
        , CyclePosition 0 2 ]
    , lifts =
        [ LiftStats "Press" 1.25 57.5 (CyclePosition 0 4) False
        , LiftStats "Bench" 1.25 96.25 (CyclePosition 0 4) False
        , LiftStats "Squat" 2.5 145 (CyclePosition 0 4) False
        , LiftStats "Deadlift" 2.5 150 (CyclePosition 0 4) False
        , LiftStats "Chin" 1.25 105 (CyclePosition 0 4) True
        , LiftStats "Row" 1.25 91.25 (CyclePosition 0 3) False ]
    , bodyweight = 72.7
    }

addWork :: Int -> Lift -> Stats -> Stats
addWork work = toLiftStats (addWorkToLift work)

addWorkToLift :: Int -> LiftStats -> LiftStats
addWorkToLift work liftStats = liftStats
    { liftCycle = addLength work $ liftCycle liftStats }

addLength :: Int -> CyclePosition -> CyclePosition
addLength n (CyclePosition pos len) = CyclePosition pos (len+n)

setPR :: Weight -> LiftStats -> LiftStats
setPR newPr liftStats =
    liftStats { pr = newPr }

setProgression :: Weight -> LiftStats -> LiftStats
setProgression newProgression liftStats =
    liftStats { progression = newProgression }

setCycle :: Int -> Int -> LiftStats -> LiftStats
setCycle pos len liftStats = 
    liftStats { liftCycle = CyclePosition pos len }

toggleBodyweight :: LiftStats -> LiftStats
toggleBodyweight liftStats =
    liftStats { isBodyweight = not $ isBodyweight liftStats }


toLiftStats :: (LiftStats -> LiftStats) -> Lift -> Stats -> Stats
toLiftStats f lift' stats =
    let maybeF liftStats | lift liftStats == lift' = f liftStats
                         | otherwise               = liftStats
    in stats { lifts = map maybeF $ lifts stats}

liftIsInStats :: Lift -> Stats -> Bool
liftIsInStats lift' stats =
    any ((== lift') . lift)
    $ lifts stats

statsOfLift :: Stats -> Lift -> LiftStats
statsOfLift stats lift' =
    head
    $ filter ((== lift') . lift)
    $ lifts stats    

--Adds bodyweight back to a pr from a log, that has had it subtracted.
accountForBodyweight :: Lift -> Weight -> Stats -> Weight
accountForBodyweight lift' weight stats =
    weight + addedWeight
    where
        LiftStats { isBodyweight } = statsOfLift stats lift'
        addedWeight = if isBodyweight
            then bodyweight stats
            else 0