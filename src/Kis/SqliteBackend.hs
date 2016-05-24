{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
module Kis.SqliteBackend
    ( withSqliteKis
    , sqliteBackend
    , SqliteBackendType(..)
    , runClient
    , runSingleClientSqlite
    , buildKisWithBackend
    )
where

import Kis.SqlBackend
import Kis.Kis

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Control
import Database.Persist.Sql as S
import Database.Persist.Sqlite
import Database.Sqlite as Sqlite hiding (config)
import Data.Pool
import Data.Monoid
import qualified Data.Text as T

data SqliteBackendType = InMemory | PoolBackendType T.Text Int

withSqliteKis ::
    SqliteBackendType
    -> KisConfig
    -> (Kis IO -> IO a)
    -> IO a
withSqliteKis backendType config f =
    sqliteBackend backendType >>= \backend -> f $ buildKisWithBackend backend config

runSingleClientSqlite ::
    SqliteBackendType
    -> KisConfig
    -> KisClient IO a
    -> IO a
runSingleClientSqlite backendType config client =
    withSqliteKis backendType config $ \kis -> runClient kis client

sqliteBackend :: (MonadCatch m, MonadBaseControl IO m, MonadIO m) => SqliteBackendType -> m (KisBackend SqliteException)
sqliteBackend backendType = do
    backend <- case backendType of
        InMemory ->
            do backend <- singleBackend ":memory:"
               return (SingleBackend backend sqliteExceptions)
        (PoolBackendType filename size) -> poolBackend filename size
    sqlMigrate backend
    return backend

sqliteExceptions :: SqliteException -> KisException
sqliteExceptions (SqliteException ErrorConstraint _ _) = ConstraintViolation
sqliteExceptions (SqliteException errorType x y) = OtherError (T.pack (show errorType) <> ": " <> x <> " " <> y)

poolBackend :: MonadIO m => T.Text -> Int -> m (KisBackend SqliteException)
poolBackend filename size =
    do pool <- liftIO $ createPool (singleBackend filename) (const $ return ()) 1 20 size
       return (PoolBackend pool sqliteExceptions)

singleBackend :: MonadIO m => T.Text -> m SqlBackend
singleBackend filename =
    liftIO $ do
        connection <- Sqlite.open filename
        stmt <- Sqlite.prepare connection "PRAGMA foreign_keys = ON;"
        void $ Sqlite.step stmt
        Sqlite.finalize stmt
        wrapConnection connection (\_ _ _ _ -> return ())
