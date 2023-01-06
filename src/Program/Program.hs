{-# LANGUAGE DeriveGeneric #-}

module Program.Program
    ( Program
        ( liftGroupCycles
        , lifts
        )
    , program
    , lift
    , liftCycle
    , prSession
    , liftList
    ) where

import Types.General
    ( Lift )
import Program.LiftGroupCycle
    ( LiftGroupCycle )
import Program.LiftCycle
    ( LiftCycle )
import qualified Program.LiftCycle as LiftCycle
    ( prSession )
import Program.Session
    ( Session )
import qualified Data.Map as Map
import Data.Binary
import GHC.Generics
    (Generic)

data Program = Program
    { liftGroupCycles :: [LiftGroupCycle]
    , lifts :: Map.Map Lift LiftCycle
    } deriving (Show, Read, Eq, Generic)

instance Binary Program

program :: [LiftGroupCycle] -> [(Lift, LiftCycle)] -> Program
program liftGroupCycles' lifts' =
    Program liftGroupCycles' $ Map.fromList lifts'

lift :: Lift -> LiftCycle -> (Lift, LiftCycle)
lift = (,)

liftCycle :: Lift -> Program -> Maybe LiftCycle
liftCycle lift program =
    Map.lookup lift $ lifts program

prSession :: Lift -> Program -> Maybe Session
prSession lift program =
    LiftCycle.prSession <$> liftCycle lift program

liftList :: Program -> [Lift]
liftList = Map.keys . lifts
