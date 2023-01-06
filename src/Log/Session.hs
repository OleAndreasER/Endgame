module Log.Session
    ( Session
    , failPR
    , hasSuccessfulPR
    ) where

import Log.Set
    ( Set (setType)
    , SetType (..)
    , failSet
    )

type Session = [Set]

failPR :: Session -> Session
failPR (prSet : workSets) =
    failSet prSet : workSets

hasSuccessfulPR :: Session -> Bool
hasSuccessfulPR (prSet : _) = PR True == setType prSet
