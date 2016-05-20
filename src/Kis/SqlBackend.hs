{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
module Kis.SqlBackend
    ( withInMemoryKis
    , inMemoryBackend
    , runClient
    )
where

import Control.Monad.Extra
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
import Control.Monad.Catch
import Database.Persist.Sql as S
import Database.Persist.Sqlite
import Database.Sqlite as Sqlite hiding (config)
import qualified Database.Esqueleto as E
import qualified Data.Text as T

import Kis.Model
import Kis.Kis

withInMemoryKis :: KisConfig -> KisClient IO a -> IO a
withInMemoryKis config client =
    inMemoryBackend >>= \backend -> runClient backend config client

runClient :: (MonadCatch m, MonadBaseControl IO m, MonadIO m)
          => SqlBackend -> KisConfig -> KisClient m a -> m a
runClient backend (KisConfig clock) client =
    runReaderT client (Kis (handleKisRequest backend) clock)

handleKisRequest :: (MonadCatch m, MonadBaseControl IO m, MonadIO m) =>
    SqlBackend -> forall a. KisAction a -> m a
handleKisRequest backend req = runSqlConn (convertSqliteException $ runAction req) backend

convertSqliteException :: MonadCatch m => m a -> m a
convertSqliteException = handle sqliteExceptions
    where
      sqliteExceptions (SqliteException ErrorConstraint _ _) = throwM ConstraintViolation
      sqliteExceptions (SqliteException errorType _ _) = throwM (OtherError (T.pack . show $ errorType))

inMemoryBackend :: MonadIO m => m SqlBackend
inMemoryBackend = liftIO $ do
    connection <- Sqlite.open ":memory:"
    void $ Sqlite.step =<< Sqlite.prepare connection "PRAGMA foreign_keys = ON;"
    backend <- wrapConnection connection (\_ _ _ _ -> return ())
    void $ runSqlConn (runMigrationSilent migrateAll) backend
    return backend

runAction :: (MonadCatch m, MonadIO m)
          => KisAction a -> ReaderT SqlBackend m a
runAction (CreateBed name) = S.insert (Bed name)
runAction (CreatePatient patient) = S.insert patient
runAction (GetPatient pid) = S.get pid
runAction GetPatients = getPatients
runAction (PlacePatient patId bedId) = S.insertUnique (PatientBed patId bedId)

getPatients :: MonadIO m => ReaderT SqlBackend m [Entity Patient]
getPatients = E.select $ E.from $ \p -> return p
