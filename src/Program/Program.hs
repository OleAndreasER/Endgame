{-# LANGUAGE DeriveGeneric #-}

module Program.Program
    ( Program
        ( liftGroupCycles )
    , LiftInfo.LiftInfo
        (..)
    , program
    , lift
    , liftCycle
    , prSession
    , liftList
    ) where

import Types.General
    ( Lift
    , Weight
    )
import Program.LiftGroupCycle
    ( LiftGroupCycle )
import Program.LiftCycle
    ( LiftCycle )
import qualified Program.LiftCycle as LiftCycle
    ( prSession )
import Program.Session
    ( Session )
import qualified Program.LiftInfo as LiftInfo
    ( LiftInfo
        (..)
    )
import qualified Data.Map as Map
import Data.Binary
import GHC.Generics
    (Generic)

data Program = Program
    { liftGroupCycles :: [LiftGroupCycle]
    , lifts :: Map.Map Lift LiftCycle
    , progression :: Map.Map Lift Weight
    , isBodyweight :: Map.Map Lift Bool
    } deriving (Show, Read, Eq, Generic)

instance Binary Program

program :: [LiftGroupCycle]
        -> [(LiftInfo.LiftInfo, LiftCycle)]
        -> Program
program liftGroupCycles liftInfos = Program
    liftGroupCycles
    (Map.fromList $ lift <$> liftInfos)
    (Map.fromList $ progression <$> liftInfos)
    (Map.fromList $ isBodyweight <$> liftInfos)
  where
    lift (info, cycle) =
        ( LiftInfo.name info, cycle )
    progression (info, _) =
        ( LiftInfo.name info, LiftInfo.progression info )
    isBodyweight (info, _) =
        ( LiftInfo.name info, LiftInfo.isBodyweight info )

lift :: LiftInfo.LiftInfo -> LiftCycle -> (LiftInfo.LiftInfo, LiftCycle)
lift = (,)

liftCycle :: Lift -> Program -> Maybe LiftCycle
liftCycle lift program =
    Map.lookup lift $ lifts program

prSession :: Lift -> Program -> Maybe Session
prSession lift program =
    LiftCycle.prSession <$> liftCycle lift program

liftList :: Program -> [Lift]
liftList = Map.keys . lifts
