{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Server.RequestTypes
    ( ActiveProfileRequest (..)
    ) where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data ActiveProfileRequest = ActiveProfileRequest
    { profileName :: String 
    } deriving (Show, Generic)

instance ToJSON ActiveProfileRequest
instance FromJSON ActiveProfileRequest
