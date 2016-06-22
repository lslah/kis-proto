{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module KisSpec
    ( spec
    )
where

import Kis

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Except
import Data.Time.Clock
import System.IO.Temp
import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "runClient" $
        it "can be parametrized" $
            runClient customKis $
                void $ createPatient $ PatientSubmit "Thomas"
    describe "runSingleClientSqlite" $ do
        it "can create a Patient" $
            runSingleClientSqlite InMemory kisConfig $ do
                pid <- createPatient $ PatientSubmit "Thomas"
                patient <-  getPatient pid
                liftIO $ liftM p_name patient `shouldBe` Just "Thomas"
        it "can place patient in bed" $
            runSingleClientSqlite InMemory kisConfig $ do
                pat <- createPatient $ PatientSubmit "Thomas"
                bed <- createBed $ BedSubmit "xy"
                patBed <- placePatient pat bed
                liftIO $ patBed `shouldBe` True
        it "can't place nonexisting patient in bed" $
            (runSingleClientSqlite InMemory kisConfig $ do
                bed <- createBed $ BedSubmit "xy"
                void $ placePatient (PatientId 1) bed)
            `shouldThrow` (== ConstraintViolation)
        it "can't place patient in nonexisting bed" $
            (runSingleClientSqlite InMemory kisConfig $ do
                pat <- createPatient $ PatientSubmit "xy"
                void $ placePatient pat (BedId 1))
            `shouldThrow` (== ConstraintViolation)
    describe "withSqliteKis" $
         it "can run two clients in sequence" $
            withSqliteKis InMemory kisConfig $ \kis -> do
                patId <- runClient kis $ createPatient $ PatientSubmit "Simon"
                pats <- runClient kis getPatients
                pats `shouldBe` [Patient patId "Simon"]
    describe "withSqlitePool" $ do
          it "can run two clients in sequence" $
             withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
                 withSqliteKis (PoolBackendType (T.pack fp) 10) kisConfig $ \kis ->
                     do patId <- runClient kis $ createPatient $ PatientSubmit "Simon"
                        pats <- runClient kis getPatients
                        liftIO $ pats `shouldBe` [Patient patId "Simon"]
          it "can run two clients in parallel" $
             withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
                 withSqliteKis (PoolBackendType (T.pack fp) 10) kisConfig $ \kis ->
                     do firstClientDone <- newEmptyMVar
                        let client1 =
                                do void $ createPatient $ PatientSubmit "Simon"
                                   liftIO $ putMVar firstClientDone ()
                            client2 =
                                do void $ createPatient $ PatientSubmit "Thomas"
                                   liftIO $ takeMVar firstClientDone
                                   pats <- getPatients
                                   liftIO $ map p_name pats `shouldBe` ["Simon"]
                        link =<< async (runClient kis client1)
                        link =<< async (runClient kis client2)

customKisFunction :: UTCTime -> KisRequest a -> IO a
customKisFunction _ (CreatePatient _) = return (PatientId 1)
customKisFunction _ _ = undefined

customKis :: Kis IO
customKis =
    Kis customKisFunction realTimeClock

kisConfig :: KisConfig IO
kisConfig = KisConfig realTimeClock

--mockNotificationSystem :: NotificationSystem IO
--mockNotificationSystem = NotificationSystem (return []) (return ()) (\_ -> return ())
