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
    withAsync (sqlBackend q) $ \_ ->
        do thomasId <- request q (createPatient "Thomas Wienecke")
           piaId <- request q (createPatient "Pia von Rosenberg")
           bedId <- request q (createBed "Bett 1")
           request q (placePatient thomasId bedId)
           request q getPatients
           void $ request q (getPatientBed piaId)

request :: Show a => TQueue Request -> Kis a -> IO a
request q action = do
    res <- newEmptyTMVarIO
    atomically $ writeTQueue q (Request action res)
    atomically $ takeTMVar res

sqlBackend :: TQueue Request -> IO ()
sqlBackend q =
    runSqlite ":memory:" $ do
        runMigration migrateAll
        loopSql q

loopSql :: TQueue Request -> ReaderT SqlBackend Bla ()
loopSql q = nextItem >>= process >> loopSql q
    where
        nextItem = liftIO $ atomically $ readTQueue q
        process (Request action resVar) = logReq action >> runKis action >>= resolve resVar
        resolve resVar res = logRes res >> (liftIO . atomically $ putTMVar resVar res)
        logReq action = liftIO $ putStrLn ("Request: " <> description action)
        logRes res = liftIO $ putStrLn ("Response: " <> show res)

type Bla = NoLoggingT (ResourceT IO)

data Kis a =
    Kis
    { description :: String
    , runKis :: ReaderT SqlBackend Bla a
    }

data Request where
    Request :: Show a => Kis a -> TMVar a -> Request

createPatient :: String -> Kis (Key Patient)
createPatient name = Kis ("Create patient " <> name) $ insert (Patient name)

createBed :: String -> Kis (Key Bed)
createBed name = Kis ("Create bed " <> name) $ insert (Bed name)

placePatient :: Key Patient -> Key Bed -> Kis (Maybe (Key PatientBed))
placePatient patientId bedId =
    Kis ("Place patient " <> show patientId <> " in bed " <> show bedId) $
        insertUnique (PatientBed patientId bedId)

getPatients :: Kis [Entity Patient]
getPatients = Kis "getPatients" $ E.select $ E.from $ \p -> return p






singleListToMaybe :: String -> [a] -> Maybe a
singleListToMaybe errMsg xs =
    case xs of
      [] -> Nothing
      [x] -> Just x
      _ -> error errMsg

uniqueSelect q = liftM (singleListToMaybe "Unique constraint violated") (E.select q)

getPatientBed :: Key Patient -> Kis (Maybe Bed)
getPatientBed id = Kis ("Get bed for patient " <> show id) $ do
    bedE <-
        uniqueSelect $ E.from $ \(b `E.InnerJoin` pb) -> do
            E.where_ (pb ^. PatientBedPatientId E.==. E.val id)
            E.on (pb ^. PatientBedBedId E.==. b ^. BedId)
            return b
    return (liftM entityVal bedE)

