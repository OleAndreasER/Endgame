module Profile.Advance.Prs
    ( advance
    ) where

import Profile.Profile
    ( Profile
    , toStats
    , logs
    , program
    )
import qualified Program.Program as Program
    ( progression )
import qualified Log.Log as Log
    ( lifts )
import Stats.Stats
    ( Stats
    , toLiftStats
    )
import Stats.LiftStats
    ( increasePr )
import Types.General
    ( Lift
    , Weight
    )
import Data.Maybe
    ( fromJust )

advance :: Profile -> Profile
advance profile =
    toStats (advancePrs increases) profile
  where
    lifts :: [Lift]
    lifts = Log.lifts $ head $ logs profile
    progressions :: [Weight]
    progressions =
        fromJust <$>
        (\lift -> Program.progression lift $ program profile) <$>
        lifts
    increases :: [(Lift, Weight)]
    increases = zip lifts progressions

advancePrs :: [(Lift, Weight)] -> Stats -> Stats
advancePrs increases stats =
    foldr (uncurry advancePr) stats increases

advancePr :: Lift -> Weight -> Stats -> Stats
advancePr lift progression =
    toLiftStats (increasePr progression) lift