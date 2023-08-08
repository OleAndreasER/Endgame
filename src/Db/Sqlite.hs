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
    , initializePresetPrograms
    , getAvailablePrograms
    , createNewProfile
    , deleteTrainingProfile
    , renameTrainingProfile
    , signUp
    , login
    , getUserId
    , getUsername
    , logOut
    , UID
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
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.List.Tools (setAt)
import Profile.NextLog (nextLog, nextLogs, addLog)
import Date (dateStr)
import Setup.Programs (programs)
import Control.Monad (forM_)
import Server.ResponseTypes (ProgramResponse (..))
import Relude (whenJust)
import Data.Password.Bcrypt (PasswordHash, Bcrypt)
import Data.Text (Text)
import Server.Password (hash, equalsHash)


type PasswordHashBcrypt = PasswordHash Bcrypt
derivePersistField "PasswordHashBcrypt"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserSession
    sessionId Text
    userId UserId
User
    name String
    email String
    password (PasswordHash Bcrypt)
    UniqueUserName name
    UniqueUserEmail email
ActiveProfile
    userId UserId Maybe
    ownerUserId String Maybe
    profile ProfileId Maybe
Profile
    userId UserId Maybe
    profileName String
    ownerUserId String Maybe
    program Program
    stats Stats
Log
    profile ProfileId
    log Log.Log
PresetProgram
    userId UserId Maybe
    name String
    program Program
    ownerUserId String Maybe
|]

type UID = Key User
type Sqlite a = SqlPersistT (LoggingT IO) a

createTables :: IO ()
createTables = runSqlite "endgame.db" $ do
    runMigration migrateAll

initializePresetPrograms :: IO ()
initializePresetPrograms =
    runSqlite "endgame.db" $
    forM_ programs $ \(name, program) -> do
        maybeProgram <- selectFirst
            [ PresetProgramName ==. name
            , PresetProgramOwnerUserId ==. Nothing
            ] []
        case maybeProgram of
            Just _ -> pure ()
            Nothing -> insert_ $ PresetProgram Nothing name program Nothing

activeProfileToDb :: Maybe UID -> String -> IO ()
activeProfileToDb userId profileName = runSqlite "endgame.db" $ do
    profile <- liftIO readProfile
    profileId <- insert $ Profile
        userId
        profileName
        Nothing
        (Profile.program profile)
        (Profile.stats profile)
    pure ()

insertNewProfile :: Maybe UID -> String -> Program -> Sqlite (Key Profile)
insertNewProfile userId profileName program = insert $ Profile
    userId
    profileName
    Nothing
    (Profile.program profile)
    (Profile.stats profile)
  where
    profile = newProfile program

getProgram :: Maybe UID -> Sqlite Program
getProgram userId = do
    profileId <- fromJust <$> getActiveProfileId userId
    Profile _ _ _ program _ <- fromJust <$> get profileId
    pure program

getStats :: Maybe UID -> Sqlite Stats
getStats userId = do
    profileId <- fromJust <$> getActiveProfileId userId
    Profile _ _ _ _ stats <- fromJust <$> get profileId
    pure stats

getLogs :: Maybe UID -> Sqlite [Log.Log]
getLogs userId = do
    profileId <- fromJust <$> getActiveProfileId userId
    logRecords <- selectList
        [ LogProfile ==. profileId
        ] [Desc LogId]
    pure ((\(Entity _ (Log _ log)) -> log) <$> logRecords)

(!!?) :: [a] -> Int -> Maybe a
xs !!? i
    | (i > -1) && (length xs > i) = Just (xs !! i)
    | otherwise = Nothing

getLog :: Maybe UID -> Int -> Sqlite (Maybe Log.Log)
getLog userId n
    | n >= 0 = do
        profileId <- fromJust <$> getActiveProfileId userId
        maybeLogRecord <- selectFirst
            [ LogProfile ==. profileId ]
            [ Desc LogId
            , OffsetBy n
            ]
        pure ((\(Entity _ (Log _ log)) -> log) <$> maybeLogRecord)
    | otherwise = pure Nothing

getLogId :: Maybe UID -> Int -> Sqlite (Maybe (Key Log))
getLogId userId n
    | n >= 0 = do
        profileId <- fromJust <$> getActiveProfileId userId
        maybeLogRecord <- selectFirst
            [ LogProfile ==. profileId ]
            [ Desc LogId
            , OffsetBy n
            ]
        pure ((\(Entity key _) -> key) <$> maybeLogRecord)
    | otherwise = pure Nothing

setProgram :: Maybe UID -> Program -> Sqlite ()
setProgram userId newProgram = do
    profileId <- fromJust <$> getActiveProfileId userId
    update profileId [ProfileProgram =. newProgram]

setStats :: Maybe UID -> Stats -> Sqlite ()
setStats userId newStats = do
    profileId <- fromJust <$> getActiveProfileId userId
    update profileId [ProfileStats =. newStats]

setLog :: Maybe UID -> Int -> Log.Log -> Sqlite ()
setLog userId n newLog = do
    profileId <- fromJust <$> getActiveProfileId userId
    maybeLogId <- getLogId userId n
    whenJust maybeLogId $ \logId ->
        replace logId (Log profileId newLog)

setActiveProfile :: Maybe UID -> String -> Sqlite ()
setActiveProfile userId profileName = do
    (Entity profileId _ ) : _ <- selectList
        [ ProfileProfileName ==. profileName
        , ProfileUserId ==. userId
        ] [LimitTo 1]
    updateWhere
        [ActiveProfileUserId ==. userId]
        [ActiveProfileProfile =. Just profileId]

newUser :: Maybe UID -> Sqlite (Key ActiveProfile)
newUser userId = insert $ ActiveProfile userId Nothing Nothing

getActiveProfileId :: Maybe UID -> Sqlite (Maybe ProfileId)
getActiveProfileId userId = do
    (Entity _ (ActiveProfile _ _ profileId)) : _ <- selectList
        [ ActiveProfileUserId ==. userId
        ] [LimitTo 1]
    pure profileId

getProfileName :: Maybe UID -> Sqlite (Maybe String)
getProfileName userId = do
    maybeProfileId <- getActiveProfileId userId
    case maybeProfileId of
        Nothing -> pure Nothing
        Just profileId -> do
            profile <- get profileId
            case profile of
                Nothing -> pure Nothing
                Just (Profile _ name _ _ _) -> pure (Just name)

getProfileNames :: Maybe UID -> Sqlite [String]
getProfileNames userId = do
    profiles <- selectList [ProfileUserId ==. userId] []
    pure ((\(Entity _ (Profile _ name _ _ _)) -> name) <$> profiles)

insertLog :: Maybe UID -> Log.Log -> Sqlite (Key Log)
insertLog userId log = do
    profileId <- fromJust <$> getActiveProfileId userId
    insert $ Log profileId log

-- Previous logs are not used to find next logs.
-- Program is never altered either.
addAndGetNextLog :: Maybe UID -> Sqlite Log.Log
addAndGetNextLog userId = do
    stats <- getStats userId
    program <- getProgram userId
    date <- liftIO dateStr
    let newProfile = addLog date $ profile program stats []
    setStats userId $ Profile.stats newProfile
    let addedLog : _ = Profile.logs newProfile
    insertLog userId addedLog
    pure addedLog

getNextLog :: Maybe UID -> Sqlite Log.Log
getNextLog userId = do
    stats <- getStats userId
    program <- getProgram userId
    pure $ nextLog $ profile program stats []

getNextLogs :: Maybe UID -> Int -> Sqlite [Log.Log]
getNextLogs userId n = do
    stats <- getStats userId
    program <- getProgram userId
    pure $ take n $ nextLogs $ profile program stats []

undoLog :: Maybe UID -> Sqlite ()
undoLog userId = do
    maybeLogId <- getLogId userId 0
    whenJust maybeLogId $ \logId -> do
        Log _ log <- fromJust <$> get logId
        stats <- getStats userId
        program <- getProgram userId
        setStats userId $ Stats.undoLog program log stats
        delete logId

getPrograms :: Maybe UID -> Sqlite [PresetProgram]
getPrograms userId = do
    programEntities <- selectList
        [ PresetProgramUserId ==. userId ]
        [ Asc PresetProgramName ]
    pure $ (\(Entity _ program) -> program) <$> programEntities

getAvailablePrograms :: Maybe UID -> Sqlite [ProgramResponse]
getAvailablePrograms userId = do
    premadePrograms <- getPrograms Nothing
    userPrograms <- getPrograms userId
    let programs = premadePrograms ++ userPrograms
    pure $ toProgramResponse <$> programs
  where
    toProgramResponse :: PresetProgram -> ProgramResponse
    toProgramResponse (PresetProgram userId name program Nothing) =
        ProgramResponse
            { name = name
            , program = program
            , wasMadeByUser = isJust userId
            }

createNewProfile :: Maybe UID -> String -> Program -> Sqlite ()
createNewProfile userId profileName program = do
    let profile = newProfile program
    profileAlreadyExists <- exists
        [ ProfileProfileName ==. profileName
        , ProfileUserId ==. userId
        ]
    if profileAlreadyExists || null profileName then pure ()
        else insert_ $ Profile
            userId
            profileName
            Nothing
            (Profile.program profile)
            (Profile.stats profile)

deleteTrainingProfile :: Maybe UID -> String -> Sqlite ()
deleteTrainingProfile userId profileName = do
    maybeProfileId <- selectFirst
        [ ProfileProfileName ==. profileName
        , ProfileUserId ==. userId
        ] []
    whenJust maybeProfileId $ \(Entity profileId _) -> do
        deleteWhere [ LogProfile ==. profileId ]
        updateWhere
            [ ActiveProfileUserId ==. userId
            , ActiveProfileProfile ==. Just profileId
            ]
            [ ActiveProfileProfile =. Nothing]
        delete profileId

renameTrainingProfile :: Maybe UID -> String -> String -> Sqlite ()
renameTrainingProfile userId oldName newName =
    updateWhere
        [ ProfileProfileName ==. oldName
        , ProfileUserId ==. userId
        ]
        [ ProfileProfileName =. newName ]

signUp :: String -> String -> Text -> Sqlite (Maybe (Key User))
signUp username email password = do
    hashedPassword <- liftIO $ hash password
    insertUnique $ User username email hashedPassword

login :: String -> Text -> Text -> Sqlite Bool
login email password sessionId = do
    maybeUser <- selectFirst [ UserEmail ==. email ] []
    case maybeUser of
        Nothing -> pure False
        Just (Entity userId (User _ _ passwordHash)) -> do
            let isCorrectPassword = equalsHash password passwordHash
            if isCorrectPassword
                then do
                    insert_ $ UserSession sessionId userId
                    pure True
                else pure False

logOut :: UID -> Sqlite ()
logOut userId = deleteWhere [ UserSessionUserId ==. userId]

getUserId :: Text -> Sqlite (Maybe (Key User))
getUserId sessionId = do
    maybeSession <- selectFirst
        [ UserSessionSessionId ==. sessionId ] []
    pure $ case maybeSession of
        Nothing -> Nothing
        Just (Entity _ (UserSession _ userId)) -> Just userId

getUsername :: UID -> Sqlite (Maybe String)
getUsername userId = do
    maybeUser <- get userId
    pure ((\(User name _ _) -> name) <$> maybeUser)
