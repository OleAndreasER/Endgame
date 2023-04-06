{-# LANGUAGE DeriveGeneric #-}

module Log.Log
    ( Log
    , label
    , log
    , session
    , liftSession
    , did
    , failPR
    , wasPr
    , lifts
    , sessions
    ) where 

import Prelude hiding
    ( log )
import Data.Maybe
    ( fromJust )
import GHC.Generics
    ( Generic )
import Data.Binary
    ( Binary )
import qualified Data.Map as Map
import Log.Session
    ( Session
    , hasSuccessfulPr
    )
import qualified Log.Session as Session
    ( failPR )
import Types.General
    ( Lift )

data Log = Log 
    { label :: String
    , sessionMap :: Map.Map Lift Session
    , liftsInOrder :: [Lift]
    } deriving (Generic, Show, Read, Eq)

instance Binary Log

log :: String -> [(Lift, Session)] -> Log
log label sessions = Log
    label
    (Map.fromList sessions)
    (fst <$> sessions)

session :: Lift -> Log -> Maybe Session
session lift log =
    Map.lookup lift $ sessionMap log

toSession :: (Session -> Session) -> Lift -> Log -> Log
toSession f lift log = log
    { sessionMap = Map.adjust f lift $ sessionMap log }

liftSession :: Lift -> Session -> (Lift, Session)
liftSession = (,)

did :: Lift -> Log -> Bool
did lift log = lift `elem` lifts log

wasPr :: Log -> Lift -> Bool
wasPr log lift =
    Just True == (hasSuccessfulPr <$> session lift log)

failPR :: Lift -> Log -> Log
failPR = toSession Session.failPR

lifts :: Log -> [Lift]
lifts = liftsInOrder

sessions :: Log -> [Session]
sessions log =
    (\lift -> fromJust $ session lift log) <$>
    liftsInOrder log
