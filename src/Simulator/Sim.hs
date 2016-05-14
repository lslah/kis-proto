{-# LANGUAGE RankNTypes             #-}
module Simulator.Sim
    ( runSimulator )
where

import Control.Monad.IO.Class
import Data.Time.Clock

import Kis
import Kis.SqlBackend
import Kis.Time
import Simulator.Template


runSimulator :: MonadIO m => Template m (PatientId, BedId) -> KisClient m ()
runSimulator template =
  do patientId <- req (CreatePatient (Patient "Simon"))
     bedId <- req (CreateBed "1a")
     loop template (patientId, bedId)
       where
         loop template templateInfo =
           case template of
            Template [] -> return ()
            Template ((SimulatorAction kisAction, timeToNextAction):rest) ->
                do waitForKisTime timeToNextAction
                   (newPatId, newBedId) <- kisAction templateInfo
                   loop (Template rest) (newPatId, newBedId)

simMain :: IO ()
simMain =
    do now <- getCurrentTime
       backend <- inMemoryBackend
       let kisConfig = KisConfig (VirtualTime now multiplier)
       runClient backend kisConfig (runSimulator __template1__)
    where
      multiplier = 2
