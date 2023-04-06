module Program.LiftCycle
    ( LiftCycle
    , prSession
    , asCycle
    ) where

import Program.Session
    ( Session )

type LiftCycle = [Session]

prSession :: LiftCycle -> Session
prSession = head

asCycle :: LiftCycle -> LiftCycle
asCycle (prSession : workSessions) = prSession : cycle workSessions
