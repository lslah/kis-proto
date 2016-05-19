{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
module Kis.SqlBackend
    ( runClient
    , sqliteBackend
    , SqliteBackendType(..)
    , withSqliteKis
    )
where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Database.Persist.Sql as S
import Database.Persist.Sqlite
import Database.Sqlite as Sqlite hiding (config)
import Data.Pool
import Data.Monoid
import qualified Database.Esqueleto as E
import qualified Data.Text as T

import Kis.Model
import Kis.Kis

withSqliteKis :: SqliteBackendType -> KisConfig -> KisClient IO a -> IO a
withSqliteKis backendType config client =
    sqliteBackend backendType >>= \backend -> runClient backend config client

runClient :: (MonadCatch m, MonadBaseControl IO m, MonadIO m)
          => Either SqlBackend ConnectionPool -> KisConfig -> KisClient m a -> m a
runClient backend (KisConfig clock) client =
    runReaderT client (Kis (handleKisRequest backend) clock)

handleKisRequest :: (MonadCatch m, MonadBaseControl IO m, MonadIO m) =>
    Either SqlBackend ConnectionPool -> forall a. KisRequest a -> m a
handleKisRequest backend req =
    runSql backend handleRequest
    where handleRequest = convertSqliteException (runAction req)

runSql :: (MonadBaseControl IO m, MonadIO m)
       => Either SqlBackend ConnectionPool -> SqlPersistT m a -> m a
runSql backend query = either (runSqlConn query) (runSqlPool query) backend

migrate' :: (MonadBaseControl IO m, MonadIO m)
       => Either SqlBackend ConnectionPool -> m ()
migrate' backend = void $ runSql backend (runMigrationSilent migrateAll)

convertSqliteException :: MonadCatch m => m a -> m a
convertSqliteException = handle sqliteExceptions
    where
      sqliteExceptions (SqliteException ErrorConstraint _ _) = throwM ConstraintViolation
      sqliteExceptions (SqliteException errorType x y) = throwM (OtherError (T.pack (show errorType) <> ": " <> x <> " " <> y))

data SqliteBackendType = InMemory | PoolBackend T.Text Int

sqliteBackend :: (MonadBaseControl IO m, MonadIO m) => SqliteBackendType -> m (Either SqlBackend ConnectionPool)
sqliteBackend backendType = do
    backend <- case backendType of
        InMemory -> liftM Left (singleBackend ":memory:")
        (PoolBackend filename size) ->
            liftM Right $
                liftIO $ createPool (singleBackend filename) (const $ return ()) 1 20 size
    migrate' backend
    return backend

singleBackend :: MonadIO m => T.Text -> m SqlBackend
singleBackend filename =
    liftIO $ do
        connection <- Sqlite.open filename
        stmt <- Sqlite.prepare connection "PRAGMA foreign_keys = ON;"
        void $ Sqlite.step stmt
        Sqlite.finalize stmt
        wrapConnection connection (\_ _ _ _ -> return ())

runAction :: (MonadCatch m, MonadIO m)
          => KisRequest a -> ReaderT SqlBackend m a
runAction (CreateBed name) = S.insert (Bed name)
runAction (CreatePatient patient) = S.insert patient
runAction (GetPatient pid) = S.get pid
runAction GetPatients = getPatients
runAction (PlacePatient patId bedId) = S.insertUnique (PatientBed patId bedId)

getPatients :: MonadIO m => ReaderT SqlBackend m [Entity Patient]
getPatients = E.select $ E.from $ \p -> return p
