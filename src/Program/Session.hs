{-# LANGUAGE DeriveGeneric #-}

module Program.Session
    ( Session
    , prSet
    ) where

import GHC.Generics
    ( Generic
    )
import Program.Set
    ( Set
    )

newtype Session = Session
    { sets :: [Set] }
    deriving (Show, Read, Eq, Generic)

prSet :: Session -> Set
prSet = head . sets
