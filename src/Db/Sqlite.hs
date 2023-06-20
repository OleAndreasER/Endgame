{-# LANGUAGE OverloadedStrings          #-}
module Db.Sqlite
    ( main
    , createTables
    ) where

import Database.Persist.Sqlite
import Database.Persist
import Db.Schema

createTables :: IO ()
createTables = runSqlite "endgame.db" $ do
    runMigration migrateAll

main :: IO ()
main = runSqlite "endgame.db" $ do
    pure ()


