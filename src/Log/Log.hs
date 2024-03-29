{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Log.Log
    ( Log
    , label
    , log
    , session
    , liftSession
    , did
    , failPr
    , wasPr
    , lifts
    , liftPrs
    , sessions
    ) where

import Prelude hiding
    ( log )
import Data.Maybe
    ( fromJust, maybeToList )
import GHC.Generics
    ( Generic )
import Data.Binary
    ( Binary )
import qualified Data.Map as Map
import Log.Session
    ( Session
    , hasSuccessfulPr
    )
import qualified Log.Session as Session ( liftPrs, failPr )
import Types.General
    ( Lift )
import Data.Aeson (ToJSON, FromJSON)
import Database.Persist.TH (derivePersistField)
import Log.Set (SetType)

data Log = Log
    { label :: String
    , sessionMap :: Map.Map Lift Session
    , liftsInOrder :: [Lift]
    } deriving (Generic, Show, Read, Eq)

instance Binary Log
instance ToJSON Log
instance FromJSON Log
derivePersistField "Log"

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

failPr :: Lift -> Log -> Maybe Log
failPr lift log
    | did lift log = do
        failedSession <- Session.failPr =<< session lift log
        pure $ toSession (const failedSession) lift log
    | otherwise    = Nothing

lifts :: Log -> [Lift]
lifts = liftsInOrder

sessions :: Log -> [Session]
sessions log =
    (\lift -> fromJust $ session lift log) <$>
    liftsInOrder log

liftPrs :: Log -> [(Lift, SetType)]
liftPrs log = do
    maybeSession <- ($ log) . session <$> lifts log
    session <- maybeToList maybeSession
    Session.liftPrs session
