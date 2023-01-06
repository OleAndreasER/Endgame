module Program.LiftInfo
    ( LiftInfo
        (..)
    ) where

import Types.General
    ( Lift
    , Weight
    )

data LiftInfo = LiftInfo
    { name :: Lift
    , progression :: Weight
    , isBodyweight :: Bool
    } deriving (Show)
