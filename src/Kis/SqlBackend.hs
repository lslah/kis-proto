{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
module Kis.SqlBackend
    ( withInMemoryKis
    )
where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.RWS
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Database.Esqueleto as E
import Database.Persist.Sql as S
import Database.Persist.Sqlite

import Kis.Model
import Kis.Kis

withInMemoryKis :: KisClient (NoLoggingT IO) () -> IO ()
withInMemoryKis client =
    runNoLoggingT $
    withSqliteConn ":memory:" $ \backend -> do
        runSqlConn (runMigration migrateAll) backend
        withKis (Kis $ kis backend) client
    where
        kis :: SqlBackend -> (forall a. KisAction a -> KisClient (NoLoggingT IO) a)
        kis backend action = lift $ runSqlRequest action backend

runSqlRequest :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => KisAction a -> SqlBackend -> m a
runSqlRequest req backend = runSqlConn (runAction req) backend

runAction :: MonadIO m => KisAction a -> ReaderT SqlBackend m a
runAction (CreateBed name) = S.insert (Bed name)
runAction (CreatePatient name) = S.insert (Patient name)
runAction (GetPatient pid) = S.get pid
runAction GetPatients = getPatients -- defined in Kis.Backend
runAction (PlacePatient patId bedId) = S.insertUnique (PatientBed patId bedId)

getPatients :: MonadIO m => ReaderT SqlBackend m [Entity Patient]
getPatients = E.select $ E.from $ \p -> return p
