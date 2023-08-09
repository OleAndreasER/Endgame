{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
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
import Data.Maybe (fromJust, isJust)
import Db.Sqlite (getLogs, getLog, getProgram, getStats, newUser, setActiveProfile, getProfileName, getProfileNames, setStats, setLog, addAndGetNextLog, getNextLog, getNextLogs, undoLog, getAvailablePrograms, createNewProfile, deleteTrainingProfile, renameTrainingProfile, login, signUp, UID, getUserId, getUsername, logOut)
import Log.Log (Log)
import Stats.Stats (Stats)
import Program.Program (Program)
import Database.Persist.Sqlite (createSqlitePool, SqlBackend, SqlPersistT, runSqlConn, PersistEntity (Key))
import Server.RequestTypes.RequestTypes (ProfileRequest(ProfileRequest), LogRequest(LogRequest), toLog, SignUpRequest (SignUpRequest))
import qualified Log.Format as Log
import Server.RequestTypes.LoginRequest (LoginRequest(LoginRequest))
import Relude (whenJust)

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

startServer :: IO ()
startServer = do
  pool <- runStdoutLoggingT $ createSqlitePool "endgame.sqlite" 5
  spockConfig <- defaultSpockCfg () (PCPool pool) ()
  runSpock 8080 (spock spockConfig app)

app :: Api
app = prehook corsHeader $ do
  hookAny OPTIONS $ const $ pure () --So that corsHeader is prehooked on any OPTIONS request.

  get "log" $ requireUser $ \userId -> do
    logs <- runSQL $ getLogs $ Just userId
    json logs
  get ("log" <//> var) $ \i -> requireUser $ \userId -> do
    log <- runSQL $ getLog (Just userId) i
    json log
  post "log" $ requireUser $ \userId -> do
    addedLog <- runSQL $ addAndGetNextLog (Just userId)
    json addedLog
  delete "log" $ requireUser $ \userId -> do
    runSQL $ undoLog $ Just userId
    json ("OK" :: String)
  put ("log" <//> var) $ \i -> requireUser $ \userId -> do
    logRequest <- jsonBody' :: ApiAction LogRequest
    case toLog logRequest of
      Nothing -> errorJson 400 "Invalid log entry."
      Just log -> do
        runSQL $ setLog (Just userId) i log
        json ("OK" :: String)
  get ("log" <//> "next") $ requireUser $ \userId -> do
    nextLog' <- runSQL $ getNextLog (Just userId)
    json nextLog'
  get ("log" <//> "next" <//> var) $ \logCount -> requireUser $ \userId -> do
    nextLogs' <- runSQL $ getNextLogs (Just userId) logCount
    json nextLogs'
  get "stats" $ requireUser $ \userId -> do
    stats <- runSQL $ getStats $ Just userId
    json stats
  put "stats" $ requireUser $ \userId -> do
    stats <- jsonBody' :: ApiAction Stats
    runSQL $ setStats (Just userId) stats
    json ("OK" :: String)
  get "program" $ requireUser $ \userId -> do
    program <- runSQL $ getProgram $ Just userId
    json program
  get "programs" $ requireUser $ \userId -> do
    programs <- runSQL $ getAvailablePrograms $ Just userId
    json programs
  get ("profiles" <//> "active") $ requireUser $ \userId -> do
    profileName <- runSQL $ getProfileName $ Just userId
    json profileName
  get "profiles" $ requireUser $ \userId -> do
    profileNames <- runSQL $ getProfileNames $ Just userId
    json profileNames
  post ("profiles" <//> var) $ \profileName -> requireUser $ \userId -> do
    program <- jsonBody' :: ApiAction Program
    runSQL $ createNewProfile (Just userId) profileName program
    json ("OK" :: String)
  delete ("profiles" <//> var) $ \profileName -> requireUser $ \userId -> do
    runSQL $ deleteTrainingProfile (Just userId) profileName
    json ("OK" :: String)
  put ("profiles" <//> var) $ \profileName -> requireUser $ \userId -> do
    ProfileRequest newProfileName <- jsonBody' :: ApiAction ProfileRequest
    runSQL $ renameTrainingProfile (Just userId) profileName newProfileName
    json ("OK" :: String)
  post "users" $ do
    SignUpRequest username email password <- jsonBody' :: ApiAction SignUpRequest
    userId <- runSQL $ signUp username email password
    if isJust userId then do
      sessionId <- getSessionId
      runSQL $ login email password sessionId
      setCookie "session" sessionId authCookieSettings
      json ("Signed in" :: String)
      else errorJson 401 "Unavailable"
  post ("users" <//> "login") $ do
    LoginRequest email password <- jsonBody' :: ApiAction LoginRequest
    sessionId <- getSessionId
    isUser <- runSQL $ login email password sessionId
    if isUser
      then do
        setCookie "session" sessionId authCookieSettings
        json ("Logged in" :: String)
      else json ("No such user" :: String)
  delete ("users" <//> "login") $ requireUser $ \userId -> do
    runSQL $ logOut userId
    deleteCookie "session"
    json ("Logged out" :: String)
  put ("users" <//> "active-training-profile") $ requireUser $ \userId -> do
    ProfileRequest profileName <- jsonBody' :: ApiAction ProfileRequest
    runSQL $ setActiveProfile (Just userId) profileName
    json ("OK" :: String)
  get ("users" <//> "name") $ requireUser $ \userId -> do
    name <- runSQL $ getUsername userId
    json name

authCookieSettings :: CookieSettings
authCookieSettings = CookieSettings
  { cs_EOL = CookieValidFor 31556926 --1 year
  , cs_HTTPOnly = True
  , cs_domain = Nothing
  , cs_path = Just "/"
  , cs_secure = False
  }

corsHeader :: ApiAction ()
corsHeader = do
    context <- getContext
    setHeader "Access-Control-Allow-Origin" "http://localhost:3000"
    setHeader "Access-Control-Allow-Credentials" "true"
    setHeader "Access-Control-Allow-Headers" "Content-Type, Authorization"
    setHeader "Access-Control-Allow-Methods" "POST, PUT, GET, DELETE, OPTIONS"
    pure context

requireUser :: (UID -> ApiAction ()) -> ApiAction ()
requireUser action = do
  maybeSessionId <- cookie "session"
  case maybeSessionId of
      Nothing -> errorJson 401 "Unauthorized"
      Just sessionId -> do
        maybeUserId <- runSQL $ getUserId sessionId
        case maybeUserId of
          Nothing -> errorJson 401 "Unauthorized"
          Just userId -> action userId

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
