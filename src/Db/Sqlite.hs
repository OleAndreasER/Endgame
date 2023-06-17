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

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    oleId <- insert $ User "Ole"
    insert $ Profile "trondheim1" oleId
    insert $ Profile "trondheim2" oleId

    profiles <- selectList [ProfileUserId ==. oleId] []
    liftIO $ print $ length (profiles :: [Entity Profile])



