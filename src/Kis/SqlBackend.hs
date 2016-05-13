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
import Control.Monad.Trans.Reader
import Database.Persist.Sql as S
import Database.Persist.Sqlite
import Database.Sqlite as Sqlite hiding (config)
import qualified Database.Esqueleto as E

import Kis.Model
import Kis.Kis

withInMemoryKis :: KisConfig -> KisClient IO a -> IO a
withInMemoryKis config client =
    inMemoryBackend >>= \backend -> runClient backend config client

runClient :: SqlBackend -> KisConfig -> KisClient IO a -> IO a
runClient backend (KisConfig clock) client =
    runReaderT client (Kis (handleKisRequest backend) clock)

handleKisRequest :: SqlBackend -> forall a. KisAction a -> IO a
handleKisRequest backend req = runSqlConn (runAction req) backend

inMemoryBackend :: IO SqlBackend
inMemoryBackend = do
    connection <- Sqlite.open ":memory:"
    void $ Sqlite.step =<< Sqlite.prepare connection "PRAGMA foreign_keys = ON;"
    backend <- wrapConnection connection (\_ _ _ _ -> return ())
    void $ runSqlConn (runMigrationSilent migrateAll) backend
    return backend

runAction :: MonadIO m => KisAction a -> ReaderT SqlBackend m a
runAction (CreateBed name) = S.insert (Bed name)
runAction (CreatePatient patient) = S.insert patient
runAction (GetPatient pid) = S.get pid
runAction GetPatients = getPatients
runAction (PlacePatient patId bedId) = S.insertUnique $ PatientBed patId bedId

getPatients :: MonadIO m => ReaderT SqlBackend m [Entity Patient]
getPatients = E.select $ E.from $ \p -> return p
