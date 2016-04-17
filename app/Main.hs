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
import Control.Monad
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Esqueleto as E
import Data.Maybe

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
    withAsync (sqlBackend q) $ \_ ->
        do thomasId <- request q (CreatePatient "Thomas Wienecke")
           request q (CreatePatient "Pia von Rosenberg")
           bedId <- request q (CreateBed "Bett 1")
           request q (PlacePatient thomasId bedId)
           print =<< request q GetPatients

sqlBackend :: TQueue Request -> IO ()
sqlBackend q =
    runSqlite ":memory:" $ do
        runMigration migrateAll
        loop
    where
        loop =
            nextItem >>= process >> loop
        nextItem = liftIO $ atomically $ readTQueue q

singleListToMaybe :: String -> [a] -> Maybe a
singleListToMaybe errMsg xs =
    case xs of
      [] -> Nothing
      [x] -> Just x
      _ -> error errMsg

uniqueSelect q = liftM (singleListToMaybe "Unique constraint violated") (E.select q)

getPatientBed :: MonadIO m => PatientId -> ReaderT SqlBackend m (Maybe Bed)
getPatientBed id = do
    bedE <-
        uniqueSelect $ E.from $ \(b `E.InnerJoin` pb) -> do
            E.where_ (pb ^. PatientBedPatientId E.==. E.val id)
            E.on (pb ^. PatientBedBedId E.==. b ^. BedId)
            return b
    return (liftM entityVal bedE)

class ReqRes a b | a -> b where
    doStuff :: MonadIO m => a -> ReaderT SqlBackend m b
    request :: TQueue Request -> a -> IO b
    request q req = do
        res <- newEmptyTMVarIO
        atomically $ writeTQueue q (Request req res)
        atomically $ takeTMVar res

newtype CreatePatient = CreatePatient String
newtype CreateBed = CreateBed String
data PlacePatient = PlacePatient (Key Patient) (Key Bed)
data GetPatients = GetPatients

instance ReqRes CreatePatient (Key Patient) where
    doStuff (CreatePatient name) = insert $ Patient name

instance ReqRes CreateBed (Key Bed) where
    doStuff (CreateBed name) = insert $ Bed name

instance ReqRes PlacePatient (Maybe (Key PatientBed)) where
    doStuff (PlacePatient patientId bedId) = insertUnique $ PatientBed patientId bedId

instance ReqRes GetPatients [Entity Patient] where
    doStuff GetPatients = E.select $ E.from $ \p -> return p

data Request where
    Request :: ReqRes a b => a -> TMVar b -> Request

process :: MonadIO m => Request -> ReaderT SqlBackend m ()
process (Request a resVar) = do
    res <- doStuff a
    resolve resVar res

resolve :: MonadIO m => TMVar a -> a -> ReaderT SqlBackend m ()
resolve resVar res = liftIO . atomically $ putTMVar resVar res
