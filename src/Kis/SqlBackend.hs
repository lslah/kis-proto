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
import Database.Sqlite hiding (config)
import Data.Maybe
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
    connection <- open ":memory:"
    backend <- wrapConnection connection (\_ _ _ _ -> return ())
    void $ runSqlConn (runMigrationSilent migrateAll) backend
    return backend

runAction :: MonadIO m => KisAction a -> ReaderT SqlBackend m a
runAction (CreateBed name) = S.insert (Bed name)
runAction (CreatePatient patient) = S.insert patient
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
