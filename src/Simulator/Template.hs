module Simulator.Template
    ( Template(..)
    , SimulatorAction(..)
    , movePatient
    , __template1__
    )
where

import Control.Monad.RWS

import Kis
import Kis.Time

newtype SimulatorAction m a = SimulatorAction (a -> KisClient m a)

data Template m a = Template [(SimulatorAction m a, TimeOffset)]

__template1__ :: MonadIO m => Template m (PatientId, BedId)
__template1__ = Template [ (SimulatorAction movePatient, TimeOffset 10) ]

movePatient :: MonadIO m => (PatientId, BedId) -> KisClient m (PatientId, BedId)
movePatient (patientId, _) =
    do bedId <- req (createBed "someBed")
       void $ req (placePatient patientId bedId)
       return (patientId, bedId)
