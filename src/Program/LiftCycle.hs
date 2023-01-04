{-# LANGUAGE DeriveGeneric #-}

module Program.LiftCycle
    ( LiftCycle
    ) where

import GHC.Generics
    ( Generic
    )
import Program.Session
    ( Session
    )


data LiftCycle = LiftCycle
    { prSession :: Session
    , workSessions :: [Session]
    } deriving (Show, Read, Eq, Generic)
