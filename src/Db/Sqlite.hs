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
    , setProgram
    , setStats
    , setLogs
    , insertNewProfile
    , activeProfileToDb
    , createTables
    , printProfile
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
import Control.Monad.Trans.Reader (ReaderT)
import Profile.Profile (newProfile)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ActiveProfile
    ownerUserId String
    profile ProfileId
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


printProfile :: Maybe String -> IO ()
printProfile owner = runSqlite "endgame.db" $ do
    program <- getProgram owner
    liftIO $ putStrLn $ Program.format program
    stats <- getStats owner
    liftIO $ putStrLn $ Stats.format stats

toDb f = runSqlite "endgame.db" f

insertNewProfile owner profileName program = insert $ Profile
    profileName
    owner
    (Profile.program profile)
    (Profile.stats profile)
    (Profile.logs profile)
  where
    profile = newProfile program

getProgram owner = do
    (Entity _ (Profile _ _ program _ _) ) : _ <- selectList [ProfileOwnerUserId ==. owner] [LimitTo 1]
    pure program

getStats owner = do
    (Entity _ (Profile _ _ _ stats _) ) : _ <- selectList [ProfileOwnerUserId ==. owner] [LimitTo 1]
    pure stats

getLogs owner = do
    (Entity _ (Profile _ _ _ _ logs) ) : _ <- selectList [ProfileOwnerUserId ==. owner] [LimitTo 1]
    pure logs

setProgram owner newProgram =
    updateWhere [ProfileOwnerUserId ==. owner] [ProfileProgram =. newProgram]

setStats owner newStats =
    updateWhere [ProfileOwnerUserId ==. owner] [ProfileStats =. newStats]

setLogs owner newLogs =
    updateWhere [ProfileOwnerUserId ==. owner] [ProfileLogs =. newLogs]
