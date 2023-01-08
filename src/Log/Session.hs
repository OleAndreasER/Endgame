module Log.Session
    ( Session
    , failPR
    , hasSuccessfulPR
    ) where

import Log.Set as Set
    ( Set (setType)
    , SetType (..)
    , fail
    )

type Session = [Set]

failPR :: Session -> Session
failPR (prSet : workSets) =
    Set.fail prSet : workSets

hasSuccessfulPR :: Session -> Bool
hasSuccessfulPR (prSet : _) = PR True == setType prSet
