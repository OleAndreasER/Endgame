module Program.Session
    ( Session
    , prSet
    ) where

import Program.Set
    ( Set )

type Session = [Set]

prSet :: Session -> Set
prSet = head
