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
    , ProgramSession
    , ProgramLiftCycle
    , Program
    , ProgramLiftInLiftGroup
    , migrateAll
    ) where

import Database.Persist.TH
import File.Profile (toProgram)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Profile
    userId String Maybe
    name String
    bodyweight Double
    program ProgramId
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
    deriving Show
ProgramSets
    lift LiftId
    sets Int
    reps Int
    percent Double
    isPr Bool
    index Int
    session ProgramSessionId
    deriving Show
ProgramSession
    ProgramLiftCycle
    index Int
    deriving Show
ProgramLiftCycle
    lift LiftId
    program ProgramId
    deriving Show
Program
    name String  
    deriving Show
ProgramLiftInLiftGroup
    lift LiftId
    cycleIndex Int
    liftGroupNumber Int
    deriving Show
|]
