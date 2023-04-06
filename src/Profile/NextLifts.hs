module Profile.NextLifts
    ( nextLifts
    ) where
import Profile.Profile
    ( Profile
    , stats
    , program
    )
import Types.General
    ( Lift )
import Program.Program
    ( liftGroupCycles )
import Stats.Stats
    ( liftGroupPositions )

nextLifts :: Profile -> [Lift]
nextLifts profile =
    uncurry (!!) <$>
    zip (cycle <$> liftGroupCycles $ program profile)
    (liftGroupPositions $ stats profile)
