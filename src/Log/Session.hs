module Log.Session
    ( Session
    , session
    , bodyweightSession
    , failPr
    , hasSuccessfulPr
    , liftPrs
    ) where

import Log.Set as Set
    ( Set (lift)
    , SetType (..)
    , fail
    , set
    , bodyweightSet
    , setType
    )
import qualified Program.Session as Program
    ( Session )
import Types.General
    ( Weight, Lift )

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

liftPrs :: Session -> [(Lift, SetType)]
liftPrs session = liftPr =<< session
  where
    liftPr :: Set -> [(Lift, SetType)]
    liftPr set = case setType set of
        Work -> []
        pr   -> [(lift set, pr)]
