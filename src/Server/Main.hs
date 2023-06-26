{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Server.Main
    ( startServer
    ) where

import Web.Spock
import Web.Spock.Config

import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import File.Profile (readLog, readLogs, readProfile, readStats, readProgram)
import Control.Monad.IO.Class (liftIO)
import Profile.NextLog (nextLog, nextLogs, addLog)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Date (dateStr)
import Data.Maybe (fromJust)
import Db.Sqlite (getLogs, toProfile, getLog, getProfile, getProgram, getStats, newUser, setActiveProfile, getProfileName, getProfileNames)
import Log.Log (Log)
import Database.Persist.Sqlite (createSqlitePool, SqlBackend, SqlPersistT, runSqlConn)

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

startServer :: IO ()
startServer = do
  pool <- runStdoutLoggingT $ createSqlitePool "endgame.db" 5
  spockConfig <- defaultSpockCfg () (PCPool pool) ()
  runSpock 8080 (spock spockConfig app)

app :: Api
app = prehook corsHeader $ do
  get ("log" <//> var) $ \userId -> do
    log <- runSQL $ getLogs $ Just userId
    json log
  post ("log" <//> var) $ \userId -> do
    dateStr' <- liftIO dateStr
    runSQL $ toProfile userId (addLog dateStr')
    addedLog <- runSQL $ getLog userId 0
    json addedLog
  get ("log" <//> "next" <//> var) $ \userId -> do
    nextLog' <- runSQL $ nextLog <$> getProfile (Just userId)
    json nextLog'
  get ("log" <//> "next" <//> var <//> var) $ \userId logCount -> do
    nextLogs' <- runSQL $ take logCount . nextLogs
      <$> getProfile (Just userId)
    json nextLogs'
  get ("stats" <//> var) $ \userId -> do
    stats <- runSQL $ getStats $ Just userId
    json stats
  get ("program" <//> var) $ \userId -> do
    program <- runSQL $ getProgram $ Just userId
    json program
  get ("profiles" <//> var <//> "active") $ \userId -> do
    profileName <- runSQL $ getProfileName $ Just userId
    json profileName
  get ("profiles" <//> var) $ \userId -> do
    profileNames <- runSQL $ getProfileNames $ Just userId
    json profileNames

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

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn
