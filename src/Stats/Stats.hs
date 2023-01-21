{-# LANGUAGE DeriveGeneric #-}

module Stats.Stats
    ( Stats
    , liftGroupPositions
    , bodyweight
    , fromProgram
    , liftStats
    , renameLift
    , setLiftGroupPosition
    , setBodyweight
    , setPr
    , setCycle
    , withLiftStats
    , hasLift
    ) where

import Data.Maybe
    ( fromJust )
import qualified Data.Map as Map
import Data.Binary
import GHC.Generics
    ( Generic )
import Stats.LiftStats
    ( LiftStats )
import qualified Stats.LiftStats as LiftStats
    ( newLiftStats
    , setPr
    , setCycle
    )
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
    , liftStatsMap :: Map.Map Lift LiftStats
    , bodyweight :: Weight
    , liftsInOrder :: [Lift]
    } deriving (Generic, Show, Eq, Read)

instance Binary Stats

fromProgram :: Program -> Stats
fromProgram program = Stats
    { liftGroupPositions = map (\_ -> 0) $ liftGroupCycles program
    , liftStatsMap = newLiftStatsMap $ liftList program
    , bodyweight = 0
    , liftsInOrder = liftList program
    }

newLiftStatsMap :: [Lift] -> Map.Map Lift LiftStats
newLiftStatsMap lifts =
    Map.fromList $ zip lifts $ repeat LiftStats.newLiftStats

liftStats :: Lift -> Stats -> Maybe LiftStats
liftStats lift stats = Map.lookup lift $ liftStatsMap stats

toLiftStats :: (LiftStats -> LiftStats) -> Lift -> Stats -> Stats
toLiftStats f lift stats = stats
    { liftStatsMap = Map.adjust f lift $ liftStatsMap stats }

setPr :: Weight -> Lift -> Stats -> Stats
setPr weight = toLiftStats (LiftStats.setPr weight)

setCycle :: Int -> Int -> Lift -> Stats -> Stats
setCycle pos len = toLiftStats (LiftStats.setCycle pos len)

setBodyweight :: Weight -> Stats -> Stats
setBodyweight bw stats = stats { bodyweight = bw }

renameLift :: Lift -> Lift -> Stats -> Stats
renameLift old new stats = case liftStats old stats of
    Nothing -> stats
    Just liftStats' -> stats
        { liftStatsMap =
            insertNew liftStats' $
            removeOld $ liftStatsMap stats
        }
  where
    insertNew = Map.insert new
    removeOld = Map.delete old

liftStatsInOrder :: Stats -> [(Lift, LiftStats)]
liftStatsInOrder stats =
    (\lift -> (lift, fromJust $ liftStats lift stats)) <$>
    liftsInOrder stats

withLiftStats :: (Lift -> LiftStats -> a) -> Stats -> [a]
withLiftStats f stats =
    (uncurry f) <$> liftStatsInOrder stats

setLiftGroupPosition :: Int -> Int -> Stats -> Maybe Stats
setLiftGroupPosition n newPosition stats
    | (length $ liftGroupPositions stats) <= n = Nothing
    | otherwise = Just $ stats
        { liftGroupPositions = xs ++ (newPosition:ys) }
  where
    (xs, _:ys)= splitAt n $ liftGroupPositions stats

hasLift :: Lift -> Stats -> Bool
hasLift lift stats = lift `elem` liftsInOrder stats
