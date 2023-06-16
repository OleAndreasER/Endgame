{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
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
import Profile.NextLog (nextLog)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist hiding (get)
import qualified Database.Persist as P
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Person json
    name Text
    age Int
    deriving Show
|]

startServer :: IO ()
startServer = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool

  spockConfig <- defaultSpockCfg () (PCPool pool) ()
  runSpock 8080 (spock spockConfig app)

app :: Api
app = prehook corsHeader $ do
  get "log" $ do
    log <- liftIO $ readLogs 200
    json log
  get "next" $ do
    nextLog' <- liftIO $ nextLog <$> readProfile
    json nextLog'
  post "people" $ do
      maybePerson <- jsonBody :: ApiAction (Maybe Person)
      case maybePerson of
        Nothing -> errorJson 1 "Failed to parse request body as Person"
        Just thePerson -> do
          newId <- runSQL $ insert thePerson
          json $ object ["result" .= String "success", "id" .= newId]
  get "people" $ do
    allPeople <- runSQL $ selectList [] [Asc PersonId]
    json allPeople


corsHeader = do
    context <- getContext
    setHeader "Access-Control-Allow-Origin" "*"
    pure context

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]
