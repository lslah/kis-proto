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
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Database.Persist.Sql as S
import Database.Persist.Sqlite
import Database.Sqlite
import Data.Maybe
import qualified Database.Esqueleto as E

import Kis.Model
import Kis.Kis

withInMemoryKis :: KisClient (NoLoggingT IO) a -> IO a
withInMemoryKis client =
    runNoLoggingT $
    withSqliteConn ":memory:" $ \backend -> do
        void $ runSqlConn (runMigrationSilent migrateAll) backend
        withKis (Kis $ kis backend) client
    where
        kis :: SqlBackend -> (forall a. KisAction a -> NoLoggingT IO a)
        kis backend action = runSqlRequest action backend

runSqlRequest :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => KisAction a -> SqlBackend -> m a
runSqlRequest req backend = runSqlConn (runAction req) backend

runClient :: SqlBackend -> KisClient IO a -> IO a
runClient backend client = do
    runReaderT client (Kis kis)
    where
        kis :: forall a. KisAction a -> IO a
        kis action = runSqlConn (runAction action) backend

inMemoryBackend :: IO SqlBackend
inMemoryBackend = do
    connection <- open ":memory:"
    backend <- wrapConnection connection (\_ _ _ _ -> return ())
    void $ runSqlConn (runMigrationSilent migrateAll) backend
    return backend

runAction :: MonadIO m => KisAction a -> ReaderT SqlBackend m a
runAction (CreateBed name) = S.insert (Bed name)
runAction (CreatePatient name) = S.insert (Patient name)
runAction (GetPatient pid) = S.get pid
runAction GetPatients = getPatients
runAction (PlacePatient patId bedId) =
    ifM (exists patId &&^ exists bedId) -- Not thread safe.
        (S.insertUnique $ PatientBed patId bedId)
        (return Nothing)

exists ::
    (PersistEntity val, MonadIO m,
      PersistStore (PersistEntityBackend val)) =>
     Key val -> ReaderT (PersistEntityBackend val) m Bool
exists key = liftM isJust (S.get key)

getPatients :: MonadIO m => ReaderT SqlBackend m [Entity Patient]
getPatients = E.select $ E.from $ \p -> return p
