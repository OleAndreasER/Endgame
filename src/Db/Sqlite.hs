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
    , getProfile
    , setProgram
    , setStats
    , setLogs
    , setLog
    , setProfile
    , toProfile
    , insertNewProfile
    , activeProfileToDb
    , createTables
    , setActiveProfile
    , newUser
    , getProfileName
    , getProfileNames
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
import Log.Log (Log)
import Profile.Profile (newProfile, profile)
import Control.Monad.Logger (LoggingT)
import Data.Maybe (fromJust)
import Data.List.Tools (setAt)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ActiveProfile
    ownerUserId String Maybe
    profile ProfileId Maybe
Profile
    profileName String
    ownerUserId String Maybe
    program Program
    stats Stats
    logs [Log]
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
        (Profile.logs profile)
    pure ()

insertNewProfile :: Maybe String -> String -> Program -> SqlPersistT (LoggingT IO) (Key Profile)
insertNewProfile owner profileName program = insert $ Profile
    profileName
    owner
    (Profile.program profile)
    (Profile.stats profile)
    (Profile.logs profile)
  where
    profile = newProfile program


getProgram :: Maybe String -> SqlPersistT (LoggingT IO) Program
getProgram owner = do
    profileId <- fromJust <$> getActiveProfileId owner
    Profile _ _ program _ _ <- fromJust <$> get profileId
    pure program

getStats :: Maybe String -> SqlPersistT (LoggingT IO) Stats
getStats owner = do
    profileId <- fromJust <$> getActiveProfileId owner
    Profile _ _ _ stats _ <- fromJust <$> get profileId
    pure stats

getLogs :: Maybe String -> SqlPersistT (LoggingT IO) [Log]
getLogs owner = do
    profileId <- fromJust <$> getActiveProfileId owner
    Profile _ _ _ _ logs <- fromJust <$> get profileId
    pure logs

getProfile :: Maybe String -> SqlPersistT (LoggingT IO) Profile.Profile
getProfile owner = do
    profileId <- fromJust <$> getActiveProfileId owner
    Profile _ _ program stats logs <- fromJust <$> get profileId
    pure $ profile program stats logs

(!!?) :: [a] -> Int -> Maybe a
xs !!? i
    | (i > -1) && (length xs > i) = Just (xs !! i)
    | otherwise = Nothing

getLog :: Maybe String -> Int -> SqlPersistT (LoggingT IO) (Maybe Log)
getLog owner n = do
    logs <- getLogs owner
    pure $ logs !!? n

setProgram :: Maybe String -> Program -> SqlPersistT (LoggingT IO) ()
setProgram owner newProgram = do
    profileId <- fromJust <$> getActiveProfileId owner
    update profileId [ProfileProgram =. newProgram]

setStats :: Maybe String -> Stats -> SqlPersistT (LoggingT IO) ()
setStats owner newStats = do
    profileId <- fromJust <$> getActiveProfileId owner
    update profileId [ProfileStats =. newStats]

setLogs :: Maybe String -> [Log] -> SqlPersistT (LoggingT IO) ()
setLogs owner newLogs = do
    profileId <- fromJust <$> getActiveProfileId owner
    update profileId [ProfileLogs =. newLogs]

setLog :: Maybe String -> Int -> Log -> SqlPersistT (LoggingT IO) ()
setLog owner i newLog = do
    logs <- getLogs owner
    setLogs owner $ setAt logs i newLog

setProfile :: Maybe String -> Profile.Profile -> SqlPersistT (LoggingT IO) ()
setProfile owner newProfile = do
    profileId <- fromJust <$> getActiveProfileId owner
    update profileId
        [ ProfileLogs =. Profile.logs newProfile
        , ProfileStats =. Profile.stats newProfile
        , ProfileProgram =. Profile.program newProfile
        ]

toProfile :: Maybe String
    -> (Profile.Profile -> Profile.Profile)
    -> SqlPersistT (LoggingT IO) ()
toProfile owner f = do
    profile <- getProfile owner
    setProfile owner $ f profile

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
                Just (Profile name _ _ _ _) -> pure (Just name)

getProfileNames :: Maybe String -> SqlPersistT (LoggingT IO) [String]
getProfileNames owner = do
    profiles <- selectList [ProfileOwnerUserId ==. owner] []
    pure ((\(Entity _ (Profile name _ _ _ _)) -> name) <$> profiles)
