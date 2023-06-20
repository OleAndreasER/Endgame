{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Db.Schema
    ( Profile
    , Lift
    , LogSets
    , ProgramSets
    , migrateAll
    ) where

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Profile
    userId String
    name String
    bodyweight Double
    deriving Show
Lift
    profile ProfileId
    name String
    pr Double
    cyclePosition Int
    cycleLength Int
    isBodyweight Bool
    progression Double
    index Int
    deriving Show
LogSets
    lift LiftId
    sets Int
    reps Int
    weight Double
    isPr Bool
    prWasSuccessful Bool
ProgramSets
    lift LiftId
    sets Int
    reps Int
    percent Double
    isPr Bool
|]
