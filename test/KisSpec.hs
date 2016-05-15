{-# LANGUAGE GADTs #-}
module KisSpec
    ( spec
    )
where

import Kis
import Kis.Time
import Simulator.Sim
import Simulator.Template

import Control.Monad
import Control.Monad.IO.Class
import Database.Persist.Sqlite
import Data.Time.Clock
import Data.Maybe
import Test.Hspec
import System.Timeout

spec :: Spec
spec = do
    describe "withKis" $
        it "can be parametrized" $
            withKis (Kis kis realTimeClock) $
                void $ req (CreatePatient $ Patient "Thomas")
    describe "withInMemoryKis" $ do
        it "can create a Patient" $
            withInMemoryKis kisConfig $ do
                pid <- req (CreatePatient $ Patient "Thomas")
                patient <- req (GetPatient pid)
                liftIO $ liftM patientName patient `shouldBe` (Just "Thomas")
        it "can place patient in bed" $
            withInMemoryKis kisConfig $ do
                pat <- req (CreatePatient $ Patient "Thomas")
                bed <- req (CreateBed "xy")
                patBed <- req (PlacePatient pat bed)
                liftIO $ patBed `shouldSatisfy` isJust
        -- Are the next two statements necessary? Should we always aim for a
        -- consistent database or deal with inconsistencies like
        -- patient-bed-relations of nonexisting entities? What happens when a
        -- patient is assigned to a deleted bed?
        it "can't place nonexisting patient in bed" $
            withInMemoryKis kisConfig $ do
                bed <- req (CreateBed "xy")
                patBed <- req (PlacePatient (toSqlKey 1) bed)
                liftIO $ patBed `shouldSatisfy` isNothing
        it "can't place patient in nonexisting bed" $
            withInMemoryKis kisConfig $ do
                pat <- req (CreatePatient $ Patient "xy")
                patBed <- req (PlacePatient pat (toSqlKey 1))
                liftIO $ patBed `shouldSatisfy` isNothing
    describe "simulator" $ do
         it "can run a simple template with high multiplier" $ do
            now <- getCurrentTime
            result <- timeout (2 * 10^3) $
                withInMemoryKis (KisConfig (virtualTimeClock now 10000)) $
                    do runSimulator __template1__
                       patients <- req GetPatients
                       liftIO $ patients `shouldSatisfy` (not . null)
                       let (Entity pid _) = head patients
                       patient <- req (GetPatient pid)
                       liftIO $ liftM patientName patient `shouldBe` (Just "Simon")
            result `shouldSatisfy` isJust
         it "simulator waits for predicted amount of time" $ do
            now <- getCurrentTime
            result <- timeout (1 * 10^3) $
                withInMemoryKis (KisConfig (virtualTimeClock now 10000)) $
                    do runSimulator __template1__
                       patients <- req GetPatients
                       liftIO $ patients `shouldSatisfy` (not . null)
                       let (Entity pid _) = head patients
                       patient <- req (GetPatient pid)
                       liftIO $ liftM patientName patient `shouldBe` (Just "Simon")
            result `shouldSatisfy` isNothing

kis :: KisAction a -> IO a
kis (CreatePatient _) = return (toSqlKey 1)
kis _ = undefined

kisConfig :: KisConfig
kisConfig = KisConfig realTimeClock

