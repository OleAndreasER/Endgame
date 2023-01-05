module Setup.NewPrograms
    ( standardHalfDays
    ) where

import qualified Data.Map as Map
import Program.Program
    ( Program (..)
    )
import Program.Set
    ( prSet
    , workSets
    )
import Program.Session
    ( Session (..)
    )

standardHalfDays :: Program
standardHalfDays = Program
    { lifts = Map.fromList
        [ ( "Press"
          , [ Session $
                prSet "Press" 3 :
                workSets "Press" 1 5 87
            , Session $
                workSets "Press" 3 5 87
            ]
          )
        ]
    , liftGroupCycles =
        [ ["Press"]
        ]
    }
