module Profile.NextLog
    ( addLog
    , nextLog
    , nextLogs
    ) where

import Profile.Profile
    ( Profile
    , program
    , stats
    , logs
    , toLogs
    )
import qualified Profile.Advance.Lifts as Lifts
    ( advance )
import qualified Profile.Advance.Prs as Prs
    ( advance )
import qualified Profile.Advance.SetTypes as SetTypes
    ( advance )
import Log.Log
    ( Log
    , liftSession
    )
import qualified Log.Log as Log
    ( log )
import Log.Session
    ( Session
    , session
    , bodyweightSession
    )
import Types.General
    ( Lift )
import Stats.Stats
    ( liftGroupPositions
    , cyclePosition
    , pr
    , bodyweight
    )
import Program.Program
    ( liftGroupCycles
    , liftCycle
    , isBodyweight
    , progression
    )
import Data.Maybe
    ( fromJust )
import Program.LiftCycle
    ( LiftCycle
    , asCycle
    )
import qualified Program.Session as Program
    ( Session )
import Profile.NextLifts
    ( nextLifts )

addLog :: String -> Profile -> Profile
addLog label profile =
    Lifts.advance $
    SetTypes.advance $
    toLogs (nextLog :)
    profile'
  where
    (nextLog, profile') = nextLog' label profile

nextLog :: Profile -> Log
nextLog = fst . nextLog' "Next: "

nextLog' :: String -> Profile -> (Log, Profile)
nextLog' label profile =
    (Log.log label liftSessions, profile')
  where
    profile' = Prs.advance profile

    liftSessions :: [(Lift, Session)]
    liftSessions =
        (\lift -> (lift, session' lift)) <$>
        nextLifts profile'

    session' :: Lift -> Session
    session' lift =
        let pr' =
                fromJust $ pr lift $ stats profile'
            progression' =
                fromJust $ progression lift $ program profile'
            isBodyweight' =
                fromJust $ isBodyweight lift $ program profile' 
            bodyweight' =
                bodyweight $ stats profile'
            programSession' = programSession lift
        in if isBodyweight'
            then bodyweightSession bodyweight' progression' pr' programSession'
            else session progression' pr' programSession'

    programSession :: Lift -> Program.Session
    programSession lift =
        liftCycle' lift !!
        fromJust (cyclePosition lift $ stats profile')

    liftCycle' :: Lift -> LiftCycle
    liftCycle' lift =
        asCycle $ fromJust $ liftCycle lift $
        program profile'

nextLogs :: Profile -> [Log]
nextLogs = nextLogs' 1

nextLogs' :: Int ->  Profile -> [Log]
nextLogs' i profile =
    let nextProfile = addLog (show i ++ ".") profile
    in
        head (logs nextProfile) :
        nextLogs' (i + 1) nextProfile
