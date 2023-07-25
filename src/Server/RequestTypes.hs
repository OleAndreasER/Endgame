{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Server.RequestTypes
    ( ProfileRequest (..)
    , LogRequest (..)
    , SetsRequest (..)
    , toLog
    ) where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Log.Log (Log, log)
import Log.Session (Session)
import Prelude hiding (log, fail)
import Log.Set (Set(Set), prSet, fail, workSets)

data ProfileRequest = ProfileRequest
    { profileName :: String
    } deriving (Show, Generic)

instance ToJSON ProfileRequest
instance FromJSON ProfileRequest

data LogRequest = LogRequest
    { label :: String
    , sessions :: [(String, [SetsRequest])]
    } deriving (Show, Generic)

instance ToJSON LogRequest
instance FromJSON LogRequest

data SetsRequest = SetsRequest
    { sets :: Int
    , reps :: Integer
    , lift :: String
    , setType :: String
    , wasSuccessfulPr :: Maybe Bool
    , weight :: Float
    } deriving (Show, Generic)

instance ToJSON SetsRequest
instance FromJSON SetsRequest

toLog :: LogRequest -> Maybe Log
toLog logRequest = log (label logRequest) <$> sessions'
 where
    sessions' :: Maybe [(String, Session)]
    sessions' = mapM toSession' $ sessions logRequest
    toSession' :: (String, [SetsRequest]) -> Maybe (String, Session)
    toSession' (lift, sessionRequest) = (,) lift <$> toSession sessionRequest

toSession :: [SetsRequest] -> Maybe Session
toSession setsRequest = concat <$> mapM toSets setsRequest
  where
    toSets :: SetsRequest -> Maybe [Set]
    toSets (SetsRequest _ reps lift "PR" (Just True) weight) =
        Just [prSet lift reps weight]
    toSets (SetsRequest _ reps lift "PR" (Just False) weight) =
        sequence [fail $ prSet lift reps weight]
    toSets (SetsRequest sets reps lift "Work" _ weight) =
        Just $ workSets lift sets reps weight
    toSets _ = Nothing
