{-# LANGUAGE RankNTypes             #-}
module Simulator.Sim
    ( runSimulator
    , simMain
    )
where

import Control.Monad.IO.Class
import Data.Time.Clock

import Kis
import Kis.Time
import Simulator.Template


runSimulator ::
    (MonadIO m, KisWrite m, KisClock m)
    => Template m (PatientId, BedId)
    -> m ()
runSimulator template =
  do patientId <- createPatient (PatientSubmit "Simon")
     bedId <- createBed (BedSubmit "1a")
     loop template (patientId, bedId)
       where
         loop template' templateInfo =
           case template' of
            Template [] -> return ()
            Template ((SimulatorAction kisAction, timeToNextAction):rest) ->
                do waitForKisTime timeToNextAction
                   (newPatId, newBedId) <- kisAction templateInfo
                   loop (Template rest) (newPatId, newBedId)

simMain :: IO ()
simMain =
    do now <- getCurrentTime
       let kisConfig = KisConfig (virtualTimeClock now multiplier)
       runSingleClientSqlite InMemory kisConfig (runSimulator __template1__)
    where
      multiplier = 2
