{-# LANGUAGE OverloadedStrings #-}
module Server.Main
    ( startServer
    ) where

import Web.Spock
import Web.Spock.Config

import Data.Aeson (ToJSON, FromJSON)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import File.Profile (readLog)
import Control.Monad.IO.Class (liftIO)

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

startServer :: IO ()
startServer = do
  spockConfig <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockConfig app)

app :: Api
app = do
  get "log" $ do
    log <- liftIO $ readLog 1
    json log
