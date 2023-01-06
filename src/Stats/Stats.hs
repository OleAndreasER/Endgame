{-# LANGUAGE DeriveGeneric #-}

module Stats.Stats
    ( Stats
        ( liftGroupPositions
        , liftStats
        , bodyweight
        )
    , fromProgram
    , statsOfLift
    , renameLift
    ) where

import qualified Data.Map as Map
import Data.Binary
import GHC.Generics
    ( Generic )
import Stats.LiftStats
    ( LiftStats )
import qualified Stats.LiftStats as LiftStats
    ( newLiftStats )
import Program.Program
    ( liftList )
import Types.General
    ( Weight
    , Lift
    )
import Program.Program
    ( Program
    , liftGroupCycles
    , liftList
    )

data Stats = Stats
    { liftGroupPositions :: [Int]
    , liftStats :: Map.Map Lift LiftStats
    , bodyweight :: Weight
    } deriving (Generic, Show, Eq, Read)

instance Binary Stats

fromProgram :: Program -> Stats
fromProgram program = Stats
    { liftGroupPositions = map (\_ -> 0) $ liftGroupCycles program
    , liftStats = newLiftStats $ liftList program
    , bodyweight = 0
    }

newLiftStats :: [Lift] -> Map.Map Lift LiftStats
newLiftStats lifts =
    Map.fromList $ zip lifts $ repeat LiftStats.newLiftStats

statsOfLift :: Lift -> Stats -> Maybe LiftStats
statsOfLift lift stats = Map.lookup lift $ liftStats stats

toLiftStats :: (LiftStats -> LiftStats) -> Lift -> Stats -> Stats
toLiftStats f lift stats = stats
    { liftStats = Map.adjust f lift $ liftStats stats }

renameLift :: Lift -> Lift -> Stats -> Stats
renameLift old new stats = case statsOfLift old stats of
    Nothing -> stats
    Just liftStats' -> stats
        { liftStats = insertNew liftStats' $ removeOld $ liftStats stats }
  where
    insertNew = Map.insert new
    removeOld = Map.delete old
