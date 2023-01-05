module Setup.NewPrograms
    ( standardHalfDays
    ) where

import qualified Data.Map as Map
import Program.Program
    ( Program (..)
    )
import Program.Set
    ( Set (Set)
    , SetType (..)
    )
import Program.Session
    ( Session (..)
    )

standardHalfDays :: Program
standardHalfDays = Program
    { lifts = Map.fromList
        [ ( "Press"
          , [ Session
                [ Set "Press" 3 100 PR
                , Set "Press" 5 87 Work
                ]
            , Session
                [ Set "Press" 3 87 Work
                , Set "Press" 3 87 Work
                , Set "Press" 3 87 Work
                ]
            ]
          )
        ]
    , liftGroupCycles =
        [ ["Press"]
        ]
    }
