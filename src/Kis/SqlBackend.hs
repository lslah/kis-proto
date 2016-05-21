{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
module Kis.SqlBackend
    ( runClient
    , sqlMigrate
    , ExceptionMap
    , KisBackend(..)
    )
where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Database.Persist.Sql as S
import qualified Database.Esqueleto as E

import Kis.Model
import Kis.Kis

-- | Every sql backend should provide a function of type 'ExceptionMap e'
-- which converts the backend specific exceptions (e.g. SqliteException)
-- to 'KisException's.
type ExceptionMap e = e -> KisException

data KisBackend e =
    SingleBackend SqlBackend (ExceptionMap e)
    | PoolBackend ConnectionPool (ExceptionMap e)

runClient :: (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m)
          => KisBackend e -> KisConfig -> KisClient m a -> m a
runClient backend (KisConfig clock) client =
    runReaderT client (Kis (handleKisRequest backend) clock)

handleKisRequest :: (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m) =>
    KisBackend e -> forall a. KisRequest a -> m a
handleKisRequest backend req =
    runSql backend handleRequest
    where handleRequest = runRequest req

convertException :: (Exception e, MonadCatch m) => (e -> KisException) -> m a -> m a
convertException exceptionMap = handle (throwM . exceptionMap)

runSql :: (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m)
       => KisBackend e -> SqlPersistT m a -> m a
runSql (SingleBackend backend exceptionMap) query =
    convertException exceptionMap (runSqlConn query backend)
runSql (PoolBackend pool exceptionMap) query =
    convertException exceptionMap (runSqlPool query pool)

sqlMigrate :: (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m)
       => KisBackend e -> m ()
sqlMigrate backend = void $ runSql backend (runMigrationSilent migrateAll)

runRequest ::
    (MonadCatch m, MonadIO m)
    => KisRequest a
    -> ReaderT SqlBackend m a
runRequest (Right kisRequest) = runReadRequest kisRequest
runRequest (Left kisRequest) = runWriteRequest kisRequest

runWriteRequest ::
    (MonadCatch m, MonadIO m)
    => KisWriteRequest a
    -> ReaderT SqlBackend m a
runWriteRequest (CreateBed name) = S.insert (Bed name)
runWriteRequest (CreatePatient patient) = S.insert patient
runWriteRequest (PlacePatient patId bedId) = S.insertUnique (PatientBed patId bedId)

runReadRequest ::
    (MonadCatch m, MonadIO m)
    => KisReadRequest a
    -> ReaderT SqlBackend m a
runReadRequest (GetPatient pid) = S.get pid
runReadRequest GetPatients = getPatientsSql

getPatientsSql :: MonadIO m => ReaderT SqlBackend m [Entity Patient]
getPatientsSql = E.select $ E.from $ \p -> return p
