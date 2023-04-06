module Profile.Advance.SetTypes
    ( advance
    ) where

import Profile.Profile
    ( Profile
    , toStats
    , logs
    )
import Log.Log
    ( lifts )
import Stats.Stats
    ( advanceCycle )
import Types.General
    ( Lift )

-- PR -> Work -> Work -> PR ->

advance :: Profile -> Profile
advance profile =
    foldr advanceLift profile $ -- Advance cycle of each lift in log
    lifts $ head $ logs profile
  where
    advanceLift :: Lift -> Profile -> Profile
    advanceLift lift = toStats (advanceCycle lift)
