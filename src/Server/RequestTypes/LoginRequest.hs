{-# LANGUAGE DeriveGeneric #-}
module Server.RequestTypes.LoginRequest
    ( LoginRequest (..)
    ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)

data LoginRequest = LoginRequest
    { email :: String
    , password :: Text
    } deriving (Show, Generic)
    
instance ToJSON LoginRequest
instance FromJSON LoginRequest
