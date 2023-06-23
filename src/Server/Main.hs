{-# LANGUAGE OverloadedStrings #-}
module Server.Main
    ( startServer
    ) where

import Web.Spock
import Web.Spock.Config

import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import File.Profile (readLog, readLogs, readProfile, readStats, readProgram, toProfile)
import Control.Monad.IO.Class (liftIO)
import Profile.NextLog (nextLog, nextLogs, addLog)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Date (dateStr)
import Data.Maybe (fromJust)

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
  post "log" $ do
    dateStr' <- liftIO dateStr
    liftIO $ toProfile (addLog dateStr')
    addedLog <- liftIO (fromJust <$> readLog 0)
    json addedLog
  get ("log" <//> "next") $ do
    nextLog' <- liftIO $ nextLog <$> readProfile
    json nextLog'
  get ("log" <//> "next" <//> var) $ \logCount -> do
    nextLogs' <- liftIO $ take logCount . nextLogs <$> readProfile
    json nextLogs'
  get "stats" $ do
    stats <- liftIO readStats
    json stats
  get "program" $ do
    program <- liftIO readProgram
    json program

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
