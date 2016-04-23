module Kis
    ( sqlThread
    , request
    , createBed
    , placePatient
    , getPatients
    , createPatient
    , getPatientBed
    , Request
    )
where

import Control.Monad
import Database.Esqueleto as E
import Data.Monoid

import Kis.Loop
import Kis.Model

createPatient :: String -> KisAction PatientId
createPatient name = KisAction ("Create patient " <> name) $ insert (Patient name)

createBed :: String -> KisAction BedId
createBed name = KisAction ("Create bed " <> name) $ insert (Bed name)

placePatient :: PatientId -> BedId -> KisAction (Maybe PatientBedId)
placePatient patientId bedId =
    KisAction ("Place patient " <> show patientId <> " in bed " <> show bedId) $
        insertUnique (PatientBed patientId bedId)

getPatients :: KisAction [Entity Patient]
getPatients = KisAction "getPatients" $ E.select $ E.from $ \p -> return p

singleListToMaybe :: String -> [a] -> Maybe a
singleListToMaybe errMsg xs =
    case xs of
      [] -> Nothing
      [x] -> Just x
      _ -> error errMsg

getPatientBed :: PatientId -> KisAction (Maybe Bed)
getPatientBed patientId = KisAction ("Get bed for patient " <> show patientId) $ do
    bedE <-
        uniqueSelect $ E.from $ \(b `E.InnerJoin` pb) -> do
            E.where_ (pb ^. PatientBedPatientId E.==. E.val patientId)
            E.on (pb ^. PatientBedBedId E.==. b ^. BedId)
            return b
    return (liftM entityVal bedE)
    where
        uniqueSelect q = liftM (singleListToMaybe "Unique constraint violated") (E.select q)
