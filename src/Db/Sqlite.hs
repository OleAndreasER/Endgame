{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Db.Sqlite
    ( getProgram
    , getStats
    , getLogs
    , getLog
    , setProgram
    , setStats
    , setLog
    , addAndGetNextLog
    , getNextLog
    , getNextLogs
    , insertNewProfile
    , activeProfileToDb
    , createTables
    , setActiveProfile
    , newUser
    , getProfileName
    , getProfileNames
    , insertLog
    , undoLog
    ) where

import Database.Persist.Sqlite
import Database.Persist
import Database.Persist.TH
import File.Profile (readProfile)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Profile.Profile as Profile
import Program.Program as Program (Program)
import qualified Program.Format as Program (format)
import Stats.Stats (Stats)
import qualified Stats.Format as Stats (format)
import qualified Stats.Stats as Stats (undoLog)
import qualified Log.Log as Log (Log)
import Profile.Profile (newProfile, profile)
import Control.Monad.Logger (LoggingT)
import Data.Maybe (fromJust)
import Data.List.Tools (setAt)
import Profile.NextLog (nextLog, nextLogs, addLog)
import Date (dateStr)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ActiveProfile
    ownerUserId String Maybe
    profile ProfileId Maybe
Profile
    profileName String
    ownerUserId String Maybe
    program Program
    stats Stats
Log
    profile ProfileId
    log Log.Log
|]

createTables :: IO ()
createTables = runSqlite "endgame.db" $ do
    runMigration migrateAll

activeProfileToDb :: Maybe String -> String -> IO ()
activeProfileToDb owner profileName = runSqlite "endgame.db" $ do
    profile <- liftIO readProfile
    profileId <- insert $ Profile
        profileName
        owner
        (Profile.program profile)
        (Profile.stats profile)
    pure ()

insertNewProfile :: Maybe String -> String -> Program -> SqlPersistT (LoggingT IO) (Key Profile)
insertNewProfile owner profileName program = insert $ Profile
    profileName
    owner
    (Profile.program profile)
    (Profile.stats profile)
  where
    profile = newProfile program

getProgram :: Maybe String -> SqlPersistT (LoggingT IO) Program
getProgram owner = do
    profileId <- fromJust <$> getActiveProfileId owner
    Profile _ _ program _ <- fromJust <$> get profileId
    pure program

getStats :: Maybe String -> SqlPersistT (LoggingT IO) Stats
getStats owner = do
    profileId <- fromJust <$> getActiveProfileId owner
    Profile _ _ _ stats <- fromJust <$> get profileId
    pure stats

getLogs :: Maybe String -> SqlPersistT (LoggingT IO) [Log.Log]
getLogs owner = do
    profileId <- fromJust <$> getActiveProfileId owner
    logRecords <- selectList
        [ LogProfile ==. profileId 
        ] [Desc LogId]
    pure ((\(Entity _ (Log _ log)) -> log) <$> logRecords)

(!!?) :: [a] -> Int -> Maybe a
xs !!? i
    | (i > -1) && (length xs > i) = Just (xs !! i)
    | otherwise = Nothing

getLog :: Maybe String -> Int -> SqlPersistT (LoggingT IO) (Maybe Log.Log)
getLog owner n
    | n >= 0 = do
        profileId <- fromJust <$> getActiveProfileId owner
        maybeLogRecord <- selectFirst
            [ LogProfile ==. profileId ]
            [ Desc LogId
            , OffsetBy n
            ]
        pure ((\(Entity _ (Log _ log)) -> log) <$> maybeLogRecord)
    | otherwise = pure Nothing

getLogId :: Maybe String -> Int -> SqlPersistT (LoggingT IO) (Maybe (Key Log))
getLogId owner n
    | n >= 0 = do
        profileId <- fromJust <$> getActiveProfileId owner
        maybeLogRecord <- selectFirst
            [ LogProfile ==. profileId ]
            [ Desc LogId
            , OffsetBy n
            ]
        pure ((\(Entity key _) -> key) <$> maybeLogRecord)
    | otherwise = pure Nothing

setProgram :: Maybe String -> Program -> SqlPersistT (LoggingT IO) ()
setProgram owner newProgram = do
    profileId <- fromJust <$> getActiveProfileId owner
    update profileId [ProfileProgram =. newProgram]

setStats :: Maybe String -> Stats -> SqlPersistT (LoggingT IO) ()
setStats owner newStats = do
    profileId <- fromJust <$> getActiveProfileId owner
    update profileId [ProfileStats =. newStats]

setLog :: Maybe String -> Int -> Log.Log -> SqlPersistT (LoggingT IO) ()
setLog owner n newLog = do
    profileId <- fromJust <$> getActiveProfileId owner
    maybeLogId <- getLogId owner n
    case maybeLogId of
        Nothing -> pure ()
        Just logId -> replace logId (Log profileId newLog)

setActiveProfile :: Maybe String -> String -> SqlPersistT (LoggingT IO) ()
setActiveProfile user profileName = do
    (Entity profileId _ ) : _ <- selectList
        [ ProfileProfileName ==. profileName
        , ProfileOwnerUserId ==. user
        ] [LimitTo 1]
    updateWhere
        [ActiveProfileOwnerUserId ==. user]
        [ActiveProfileProfile =. Just profileId]

newUser :: Maybe String -> SqlPersistT (LoggingT IO) (Key ActiveProfile)
newUser user = insert $ ActiveProfile user Nothing

getActiveProfileId :: Maybe String -> SqlPersistT (LoggingT IO) (Maybe ProfileId)
getActiveProfileId user = do
    (Entity _ (ActiveProfile _ profileId)) : _ <- selectList
        [ ActiveProfileOwnerUserId ==. user
        ] [LimitTo 1]
    pure profileId

getProfileName :: Maybe String -> SqlPersistT (LoggingT IO) (Maybe String)
getProfileName owner = do
    maybeProfileId <- getActiveProfileId owner
    case maybeProfileId of
        Nothing -> pure Nothing
        Just profileId -> do
            profile <- get profileId
            case profile of
                Nothing -> pure Nothing
                Just (Profile name _ _ _) -> pure (Just name)

getProfileNames :: Maybe String -> SqlPersistT (LoggingT IO) [String]
getProfileNames owner = do
    profiles <- selectList [ProfileOwnerUserId ==. owner] []
    pure ((\(Entity _ (Profile name _ _ _)) -> name) <$> profiles)

insertLog :: Maybe String -> Log.Log -> SqlPersistT (LoggingT IO) (Key Log)
insertLog owner log = do
    profileId <- fromJust <$> getActiveProfileId owner
    insert $ Log profileId log

-- Previous logs are not used to find next logs.
-- Program is never altered either.
addAndGetNextLog :: Maybe String -> SqlPersistT (LoggingT IO) Log.Log
addAndGetNextLog owner = do
    stats <- getStats owner
    program <- getProgram owner
    date <- liftIO dateStr
    let newProfile = addLog date $ profile program stats []
    setStats owner $ Profile.stats newProfile
    let addedLog : _ = Profile.logs newProfile
    insertLog owner addedLog
    pure addedLog

getNextLog :: Maybe String -> SqlPersistT (LoggingT IO) Log.Log
getNextLog owner = do
    stats <- getStats owner
    program <- getProgram owner
    pure $ nextLog $ profile program stats []

getNextLogs :: Maybe String -> Int -> SqlPersistT (LoggingT IO) [Log.Log]
getNextLogs owner n = do
    stats <- getStats owner
    program <- getProgram owner
    pure $ take n $ nextLogs $ profile program stats []

undoLog :: Maybe String -> SqlPersistT (LoggingT IO) ()
undoLog owner = do
    maybeLogId <- getLogId owner 0
    case maybeLogId of
        Nothing -> pure ()
        Just logId -> do
            Log _ log <- fromJust <$> get logId
            stats <- getStats owner
            setStats owner $ Stats.undoLog log stats
            delete logId

