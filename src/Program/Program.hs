{-# LANGUAGE DeriveGeneric #-}
module Program.Program
    (
    ) where

import Types.General
    ( Lift
    )
import Program.LiftGroupCycle
    ( LiftGroupCycle
    )
import Data.HashMap.Strict
import GHC.Generics (Generic)

data Program = Program
    { lifts :: HashMap Lift LiftCycle
    , liftGroupCycles :: [LiftGroupCycle]
    } deriving (Show, Read, Eq, Generic)

type LiftCycle = Int

