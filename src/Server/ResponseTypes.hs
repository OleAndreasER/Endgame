{-# LANGUAGE DeriveGeneric #-}
module Server.ResponseTypes
    ( ProgramResponse (..)
    ) where
import GHC.Generics (Generic)
import Program.Program (Program)
import Data.Aeson (FromJSON, ToJSON)
    
data ProgramResponse = ProgramResponse
    { name :: String
    , program :: Program
    , wasMadeByUser :: Bool
    } deriving (Show, Generic)

instance ToJSON ProgramResponse
instance FromJSON  ProgramResponse 
