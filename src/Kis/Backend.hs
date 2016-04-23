module Kis.Backend
    ( createBed
    , placePatient
    , getPatients
    , createPatient
    , getPatientBed
    )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.Esqueleto as E

import Kis.Model

createPatient :: MonadIO m => String -> ReaderT SqlBackend m PatientId
createPatient name = insert (Patient name)

createBed :: MonadIO m => String -> ReaderT SqlBackend m BedId
createBed name = insert (Bed name)

placePatient :: MonadIO m => PatientId -> BedId -> ReaderT SqlBackend m (Maybe PatientBedId)
placePatient patientId bedId = insertUnique (PatientBed patientId bedId)

getPatients :: MonadIO m => ReaderT SqlBackend m [Entity Patient]
getPatients = E.select $ E.from $ \p -> return p

singleListToMaybe :: String -> [a] -> Maybe a
singleListToMaybe errMsg xs =
    case xs of
      [] -> Nothing
      [x] -> Just x
      _ -> error errMsg

getPatientBed :: MonadIO m => PatientId -> ReaderT SqlBackend m (Maybe Bed)
getPatientBed patientId = do
    bedE <-
        uniqueSelect $ E.from $ \(b `E.InnerJoin` pb) -> do
            E.where_ (pb ^. PatientBedPatientId E.==. E.val patientId)
            E.on (pb ^. PatientBedBedId E.==. b ^. BedId)
            return b
    return (liftM entityVal bedE)
    where
        uniqueSelect q = liftM (singleListToMaybe "Unique constraint violated") (E.select q)
