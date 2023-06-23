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
    ( main
    , createTables
    , printProfile
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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ActiveProfile
    ownerUserId String
    profile ProfileId
Profile
    name String
    ownerUserId String Maybe
    program Program
    stats Stats
    logs [Log]
|]

createTables :: IO ()
createTables = runSqlite "endgame.db" $ do
    runMigration migrateAll

main :: IO ()
main = runSqlite "endgame.db" $ do
    profile <- liftIO readProfile
    profileId <- insert $ Profile
        "Trondheim renaissance" 
        Nothing
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

getProgram owner = do
    (Entity _ (Profile _ _ program _ _) ) : _ <- selectList [ProfileOwnerUserId ==. owner] [LimitTo 1]
    pure program

getStats owner = do
    (Entity _ (Profile _ _ _ stats _) ) : _ <- selectList [ProfileOwnerUserId ==. owner] [LimitTo 1]
    pure stats
    
getLogs owner = do
    (Entity _ (Profile _ _ _ _ logs) ) : _ <- selectList [ProfileOwnerUserId ==. owner] [LimitTo 1]
    pure logs
