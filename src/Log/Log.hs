{-# LANGUAGE DeriveGeneric #-}

module Log.Log
    ( Log
        ( label
        )
    , log
    , session
    , liftSession
    , did
    , failPR
    , lifts
    ) where 


import Prelude hiding
    ( log
    )
import GHC.Generics
    ( Generic
    )
import Data.Binary
import qualified Data.Map as Map
import Log.Session
    ( Session
    )
import qualified Log.Session as Session
    ( failPR
    )
import Types.General
    ( Lift
    )

data Log = Log 
    { label :: String
    , sessions :: Map.Map Lift Session
    } deriving (Generic, Show, Read, Eq)

instance Binary Log

log :: String -> [(Lift, Session)] -> Log
log label sessions = Log label $ Map.fromList sessions

session :: Lift -> Log -> Maybe Session
session lift log =
    Map.lookup lift $ sessions log

toSession :: (Session -> Session) -> Lift -> Log -> Log
toSession f lift log = log
    { sessions = Map.adjust f lift $ sessions log
    }

liftSession :: Lift -> Session -> (Lift, Session)
liftSession = (,)

did :: Lift -> Log -> Bool
did lift log = lift `elem` lifts log

failPR :: Lift -> Log -> Log
failPR = toSession Session.failPR

lifts :: Log -> [Lift]
lifts log = Map.keys $ sessions log
