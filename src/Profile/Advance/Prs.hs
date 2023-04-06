module Profile.Advance.Prs
    ( advance
    ) where

import Profile.Profile
    ( Profile 
    , stats
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
    , timeForPr
    )
import Stats.LiftStats
    ( increasePr )
import Types.General
    ( Lift
    , Weight
    )
import Data.Maybe
    ( fromJust )
import Log.Log
    ( Log
    , wasPr
    )
import Profile.NextLifts
    ( nextLifts )

-- 130kg -> 132.5kg

advance :: Profile -> Profile
advance profile =
    toStats (advancePrs increases) profile
  where
    prLifts :: [Lift]
    prLifts = filter (timeForPr $ stats profile) $ nextLifts profile

    progressions :: [Weight]
    progressions =
        fromJust .
        (\lift -> Program.progression lift $ program profile) <$>
        prLifts

    increases :: [(Lift, Weight)]
    increases = zip prLifts progressions

advancePrs :: [(Lift, Weight)] -> Stats -> Stats
advancePrs increases stats =
    foldr (uncurry advancePr) stats increases

advancePr :: Lift -> Weight -> Stats -> Stats
advancePr lift progression =
    toLiftStats (increasePr progression) lift
