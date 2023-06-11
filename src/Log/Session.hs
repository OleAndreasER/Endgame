module Log.Session
    ( Session
    , session
    , bodyweightSession
    , failPr
    , hasSuccessfulPr
    ) where

import Log.Set as Set
    ( Set (setType)
    , SetType (..)
    , fail
    , set
    , bodyweightSet
    )
import qualified Program.Session as Program
    ( Session )
import Types.General
    ( Weight )

type Session = [Set]

session :: Weight -> Weight -> Program.Session -> Session
session progression pr programSession = 
    set progression pr <$> programSession

bodyweightSession :: Weight -> Weight -> Weight -> Program.Session -> Session
bodyweightSession bodyweight progression pr programSession =
    bodyweightSet bodyweight progression pr <$> programSession

failPr :: Session -> Maybe Session
failPr (prSet : workSets) =
    (: workSets) <$> Set.fail prSet

hasSuccessfulPr :: Session -> Bool
hasSuccessfulPr (maybePrSet : _) = PR True == setType maybePrSet
