module Stats.Format
    ( format
    ) where

import Stats.Stats
    ( Stats
    , bodyweight
    , withLiftStats
    )
import Stats.LiftStats
    ( LiftStats
    , pr
    , cyclePosition
    , cycleLength
    )
import Types.General
    ( Lift )

format :: Stats -> String
format stats = 
    init $ unlines $
    ("Bodyweight: " ++ show (bodyweight stats) ++ "kg") :
    withLiftStats formatLiftStats stats

formatLiftStats :: Lift -> LiftStats -> String
formatLiftStats lift liftStats =
    lift ++ ": " ++
    show (pr liftStats) ++ "kg " ++
    show (1 + cyclePosition liftStats) ++ "/" ++
    show (cycleLength liftStats)
