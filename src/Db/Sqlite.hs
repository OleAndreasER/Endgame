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
    , setProfile
    , toProfile
    , insertNewProfile
    , activeProfileToDb
    , createTables
    , toDb
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

toDb f = runSqlite "endgame.db" f

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
    (Entity _ (Profile _ _ program _ _) ) : _ <- selectList [ProfileOwnerUserId ==. owner] [LimitTo 1]
    pure program

getStats :: Maybe String -> SqlPersistT (LoggingT IO) Stats
getStats owner = do
    (Entity _ (Profile _ _ _ stats _) ) : _ <- selectList [ProfileOwnerUserId ==. owner] [LimitTo 1]
    pure stats

getLogs :: Maybe String -> SqlPersistT (LoggingT IO) [Log]
getLogs owner = do
    (Entity _ (Profile _ _ _ _ logs) ) : _ <- selectList [ProfileOwnerUserId ==. owner] [LimitTo 1]
    pure logs

getProfile :: Maybe String -> SqlPersistT (LoggingT IO) Profile.Profile
getProfile owner = do
    (Entity _ (Profile _ _ program stats logs) ) : _ <- selectList [ProfileOwnerUserId ==. owner] [LimitTo 1]
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
setProgram owner newProgram =
    updateWhere [ProfileOwnerUserId ==. owner] [ProfileProgram =. newProgram]

setStats :: Maybe String -> Stats -> SqlPersistT (LoggingT IO) ()
setStats owner newStats =
    updateWhere [ProfileOwnerUserId ==. owner] [ProfileStats =. newStats]

setLogs :: Maybe String -> [Log] -> SqlPersistT (LoggingT IO) ()
setLogs owner newLogs =
    updateWhere [ProfileOwnerUserId ==. owner] [ProfileLogs =. newLogs]

setProfile :: Maybe String -> Profile.Profile -> SqlPersistT (LoggingT IO) ()
setProfile owner newProfile =
    updateWhere [ProfileOwnerUserId ==. owner]
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
