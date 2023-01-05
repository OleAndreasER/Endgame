{-# LANGUAGE DeriveGeneric #-}
module Program.Program
    ( Program (..)
    , liftCycle
    , prSession
    ) where

import Types.General
    ( Lift
    )
import Program.LiftGroupCycle
    ( LiftGroupCycle
    )
import Program.LiftCycle
    ( LiftCycle
    )
import qualified Program.LiftCycle as LiftCycle
    ( prSession
    )
import Program.Session
    ( Session
    )
import qualified Data.Map as Map
import GHC.Generics (Generic)

data Program = Program
    { lifts :: Map.Map Lift LiftCycle
    , liftGroupCycles :: [LiftGroupCycle]
    } deriving (Show, Read, Eq, Generic)

liftCycle :: Lift -> Program -> Maybe LiftCycle
liftCycle lift program =
    Map.lookup lift $ lifts program

prSession :: Lift -> Program -> Maybe Session
prSession lift program =
    LiftCycle.prSession <$> liftCycle lift program
