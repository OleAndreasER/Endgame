module Program.LiftCycle
    ( LiftCycle
    , prSession
    ) where

import Program.Session
    ( Session )

type LiftCycle = [Session]

prSession :: LiftCycle -> Session
prSession = head
