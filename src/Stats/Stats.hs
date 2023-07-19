{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Stats.Stats
    ( Stats
    , liftGroupPositions
    , bodyweight
    , fromProgram
    , liftStats
    , toLiftStats
    , renameLift
    , setLiftGroupPosition
    , toLiftGroupPositions
    , setBodyweight
    , setPr
    , setCycle
    , withLiftStats
    , hasLift
    , Stats.Stats.liftList
    , advanceCycle
    , cyclePosition
    , pr
    , timeForPr
    , toCycle
    , undoLog
    ) where

import Data.Maybe
    ( fromJust, maybeToList, fromMaybe )
import qualified Data.Map as Map
import Data.Binary ( Binary )
import GHC.Generics
    ( Generic )
import Stats.LiftStats
    ( LiftStats (cycleLength) )
import qualified Stats.LiftStats as LiftStats
    ( newLiftStats
    , setPr
    , setCycle
    , advanceCycle
    , cyclePosition
    , pr
    )
import Types.General
    ( Weight
    , Lift
    )
import qualified Program.Program as Program
    ( liftList )
import Program.Program
    ( Program
    , liftGroupCycles
    , liftList, progression
    )
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.TH (derivePersistField)
import Log.Log (Log, lifts, session, liftPrs)
import Log.Set (SetType (..))
import Data.Foldable (foldrM)

data Stats = Stats
    { liftGroupPositions :: [Int]
    , liftStatsMap :: Map.Map Lift LiftStats
    , bodyweight :: Weight
    , liftsInOrder :: [Lift]
    } deriving (Generic, Show, Eq, Read)

instance Binary Stats
instance ToJSON Stats
instance FromJSON Stats
derivePersistField "Stats"

fromProgram :: Program -> Stats
fromProgram program = Stats
    { liftGroupPositions = map (const 0) $ liftGroupCycles program
    , liftStatsMap = newLiftStatsMap $ Program.liftList program
    , bodyweight = 0
    , liftsInOrder = Program.liftList program
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

advanceCycle :: Lift -> Stats -> Stats
advanceCycle = toLiftStats LiftStats.advanceCycle

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
    uncurry f <$> liftStatsInOrder stats

setLiftGroupPosition :: Int -> Int -> Stats -> Maybe Stats
setLiftGroupPosition n newPosition stats
    | length (liftGroupPositions stats) <= n = Nothing
    | otherwise = Just $ stats
        { liftGroupPositions = xs ++ (newPosition:ys) }
  where
    (xs, _:ys)= splitAt n $ liftGroupPositions stats

hasLift :: Lift -> Stats -> Bool
hasLift lift stats = lift `elem` liftsInOrder stats

liftList :: Stats -> [Lift]
liftList = liftsInOrder

toLiftGroupPositions :: ([Int] -> [Int]) -> Stats -> Stats
toLiftGroupPositions f stats = stats
    { liftGroupPositions = f $ liftGroupPositions stats }

cyclePosition :: Lift -> Stats -> Maybe Int
cyclePosition lift stats =
    LiftStats.cyclePosition <$>
    liftStats lift stats

timeForPr :: Stats -> Lift -> Bool
timeForPr stats lift = Just 0 == cyclePosition lift stats

pr :: Lift -> Stats -> Maybe Weight
pr lift stats =
    LiftStats.pr <$>
    liftStats lift stats

toCycle :: (Int -> Int -> (Int, Int)) -> Lift -> Stats -> Maybe Stats
toCycle f lift stats = do
    liftStats' <- liftStats lift stats
    position <- cyclePosition lift stats
    let length = cycleLength liftStats'
    let (newPosition, newLength) = f position length
    pure $ setCycle newPosition newLength lift stats

undoLiftCycles :: Log -> Stats -> Stats
undoLiftCycles log stats =
    foldr decrementCyclePosition (undoCycleLengthAdds stats) $ lifts log
  where
    decrementCyclePosition :: Lift -> Stats -> Stats
    decrementCyclePosition lift stats =
        fromMaybe stats $
        toCycle (\position length ->
            ((position - 1) `mod` length, length)) lift stats
    undoCycleLengthAdds :: Stats -> Stats
    undoCycleLengthAdds stats = foldr maybeUndoLength stats $ liftPrs log
    maybeUndoLength :: (Lift, SetType) -> Stats -> Stats
    maybeUndoLength (lift, PR False) stats = fromMaybe stats $
        toCycle (\pos len ->
            let newLen = max 1 (len-1)
            in (pos `mod` newLen, newLen)) lift stats
    maybeUndoLength _ stats = stats


undoLiftGroupCycles :: Program -> Stats -> Stats
undoLiftGroupCycles program stats =
    stats { liftGroupPositions = decrementCyclePosition <$> cycles }
 where
    cycles :: [(Int, Int)]
    cycles = zip
        (liftGroupPositions stats)
        (length <$> liftGroupCycles program)
    decrementCyclePosition :: (Int, Int) -> Int
    decrementCyclePosition (position, length) = (position - 1) `mod` length

undoPrs :: Program -> Log -> Stats -> Stats
undoPrs program log stats =
    foldr adjustPr stats $ liftPrs log
  where
    adjustPr :: (Lift, SetType) -> Stats -> Stats
    adjustPr (lift, setType) = setPr
        (increase lift setType + fromMaybe 0 (pr lift stats)) lift
    increase :: Lift -> SetType -> Weight
    increase _ Work = 0
    increase lift (PR True) = -(fromMaybe 0 $ progression lift program)
    increase lift (PR False) = fromMaybe 0 $ progression lift program

undoLog :: Program -> Log -> Stats -> Stats
undoLog program log stats =
    undoLiftCycles log $
    undoLiftGroupCycles program $
    undoPrs program log stats

