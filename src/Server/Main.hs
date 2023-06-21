{-# LANGUAGE OverloadedStrings #-}
module Server.Main
    ( startServer
    ) where

import Web.Spock
import Web.Spock.Config

import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import File.Profile (readLog, readLogs, readProfile)
import Control.Monad.IO.Class (liftIO)
import Profile.NextLog (nextLog, nextLogs)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

startServer :: IO ()
startServer = do
  spockConfig <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockConfig app)

app :: Api
app = prehook corsHeader $ do
  get "log" $ do
    log <- liftIO $ readLogs 200
    json log
  get ("log" <//> "next") $ do
    nextLog' <- liftIO $ nextLog <$> readProfile
    json nextLog'
  get ("log" <//> "next" <//> var) $ \logCount -> do
    nextLogs' <- liftIO $ take logCount . nextLogs <$> readProfile
    json nextLogs'

corsHeader = do
    context <- getContext
    setHeader "Access-Control-Allow-Origin" "*"
    pure context

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]
