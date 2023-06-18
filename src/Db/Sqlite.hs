{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Db.Sqlite
    ( main
    , createTables
    ) where

import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist
import Control.Monad.IO.Class (MonadIO(liftIO))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    deriving Show
Profile
    name String
    userId UserId
    deriving Show
|]

createTables :: IO ()
createTables = runSqlite "endgame.db" $ do
    runMigration migrateAll

main :: IO ()
main = runSqlite "endgame.db" $ do
    (Entity oleId ole : _) <- selectList [] [LimitTo 1]
    profiles <- selectList [ProfileUserId ==. oleId] []
    liftIO $ mapM_ print ((\ (Entity profileId profile) -> profile) <$> profiles)
