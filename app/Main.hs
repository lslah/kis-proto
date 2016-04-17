{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}

module Main where

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent.Async
import Control.Monad.STM
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
import Control.Monad
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Esqueleto as E
import Data.Maybe
import Data.Monoid
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Patient
    name String
    deriving Show
Bed
    name String
    deriving Show

PatientBed
    patientId PatientId
    bedId BedId
    UniquePatientId patientId
    UniqueBedId bedId
    deriving Show
|]

main :: IO ()
main = do
    q <- newTQueueIO
    withAsync
        (runNoLoggingT $ withSqliteConn ":memory:" $ \backend -> sqlThread backend q)
        (\_ -> main' q)

main' :: TQueue (Request IO) -> IO ()
main' q = do
    thomasId <- request q (createPatient "Thomas")
    bedId <- request q (createBed "Thomas' Bed")
    request q (createPatient "Pia")
    request q (placePatient thomasId bedId)
    void $ request q getPatients

request :: Show a => TQueue (Request IO) -> Kis IO a -> IO a
request q action = do
    res <- newEmptyTMVarIO
    atomically $ writeTQueue q (Request action res)
    atomically $ takeTMVar res

sqlThread :: (MonadBaseControl IO m, MonadIO m) => SqlBackend -> TQueue (Request m) -> NoLoggingT m ()
sqlThread backend q = NoLoggingT $ runSqlConn (runMigration migrateAll >> loopSql q) backend

loopSql :: (MonadIO m, MonadBaseControl IO m) => TQueue (Request m) -> SqlPersistT m ()
loopSql q = nextItem >>= process >> loopSql q
    where
        nextItem = liftIO $ atomically $ readTQueue q
        process (Request (Kis desc action) resVar) = logReq desc >> action >>= resolve resVar
        resolve resVar res = logRes res >> (liftIO . atomically $ putTMVar resVar res)
        logReq desc = liftIO $ putStrLn ("Request: " <> desc)
        logRes res = liftIO $ putStrLn ("Response: " <> show res)

data Request m where
    Request :: Show a => Kis m a -> TMVar a -> Request m

data Kis m a where
    Kis :: String -> SqlPersistT m a -> Kis m a


createPatient :: MonadIO m => String -> Kis m (Key Patient)
createPatient name = Kis ("Create patient " <> name) $ insert (Patient name)

createBed :: MonadIO m => String -> Kis m (Key Bed)
createBed name = Kis ("Create bed " <> name) $ insert (Bed name)

placePatient :: MonadIO m => Key Patient -> Key Bed -> Kis m (Maybe (Key PatientBed))
placePatient patientId bedId =
    Kis ("Place patient " <> show patientId <> " in bed " <> show bedId) $
        insertUnique (PatientBed patientId bedId)

getPatients :: MonadIO m => Kis m [Entity Patient]
getPatients = Kis "getPatients" $ E.select $ E.from $ \p -> return p






singleListToMaybe :: String -> [a] -> Maybe a
singleListToMaybe errMsg xs =
    case xs of
      [] -> Nothing
      [x] -> Just x
      _ -> error errMsg

uniqueSelect q = liftM (singleListToMaybe "Unique constraint violated") (E.select q)

getPatientBed :: MonadIO m => Key Patient -> Kis m (Maybe Bed)
getPatientBed id = Kis ("Get bed for patient " <> show id) $ do
    bedE <-
        uniqueSelect $ E.from $ \(b `E.InnerJoin` pb) -> do
            E.where_ (pb ^. PatientBedPatientId E.==. E.val id)
            E.on (pb ^. PatientBedBedId E.==. b ^. BedId)
            return b
    return (liftM entityVal bedE)

