{-# LANGUAGE DeriveGeneric #-}

module Program.Session
    ( Session
    , prSet
    ) where

import GHC.Generics
    ( Generic )
import Program.Set
    ( Set )

type Session = [Set]

prSet :: Session -> Set
prSet = head
