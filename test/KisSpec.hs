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
import Database.Persist.Sqlite
import Data.Maybe
import Data.Time.Clock
import System.IO.Temp
import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "runClient" $
        it "can be parametrized" $
            runClient customKis $
                void $ createPatient $ Patient "Thomas"
    describe "runSingleClientSqlite" $ do
        it "can create a Patient" $
            runSingleClientSqlite InMemory kisConfig $ do
                pid <- createPatient $ Patient "Thomas"
                patient <-  getPatient pid
                liftIO $ liftM patientName patient `shouldBe` (Just "Thomas")
        it "can place patient in bed" $
            runSingleClientSqlite InMemory kisConfig $ do
                pat <- createPatient $ Patient "Thomas"
                bed <- createBed "xy"
                patBed <- placePatient pat bed
                liftIO $ patBed `shouldSatisfy` isJust
        it "can't place nonexisting patient in bed" $
            (runSingleClientSqlite InMemory kisConfig $ do
                bed <- createBed "xy"
                void $ placePatient (toSqlKey 1) bed)
            `shouldThrow` (== ConstraintViolation)
        it "can't place patient in nonexisting bed" $
            (runSingleClientSqlite InMemory kisConfig $ do
                pat <- createPatient $ Patient "xy"
                void $ placePatient pat (toSqlKey 1))
            `shouldThrow` (== ConstraintViolation)
    describe "withSqliteKis" $
         it "can run two clients in sequence" $
            withSqliteKis InMemory kisConfig $ \kis -> do
                patId <- runClient kis $ createPatient $ Patient "Simon"
                pats <- runClient kis $ getPatients
                pats `shouldBe` [Entity patId (Patient "Simon")]
    describe "withSqlitePool" $ do
          it "can run two clients in sequence" $
             withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
                 withSqliteKis (PoolBackendType (T.pack fp) 10) kisConfig $ \kis ->
                     do patId <- runClient kis $ createPatient $ Patient "Simon"
                        pats <- runClient kis $ getPatients
                        liftIO $ pats `shouldBe` [Entity patId (Patient "Simon")]
          it "can run two clients in parallel" $
             withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
                 withSqliteKis (PoolBackendType (T.pack fp) 10) kisConfig $ \kis ->
                     do firstClientDone <- newEmptyMVar
                        let client1 =
                                do void $ createPatient $ Patient "Simon"
                                   liftIO $ putMVar firstClientDone ()
                            client2 =
                                do void $ createPatient $ Patient "Thomas"
                                   liftIO $ takeMVar firstClientDone
                                   pats <- getPatients
                                   liftIO $ map (\(Entity _ pat) -> pat) pats `shouldBe` [Patient "Simon"]
                        link =<< async (runClient kis client1)
                        link =<< async (runClient kis client2)

customKisFunction :: UTCTime -> KisRequest a -> IO a
customKisFunction _ (CreatePatient _) = return (toSqlKey 1)
customKisFunction _ _ = undefined

customKis :: Kis IO
customKis =
    Kis customKisFunction realTimeClock

kisConfig :: KisConfig IO
kisConfig = KisConfig realTimeClock

--mockNotificationSystem :: NotificationSystem IO
--mockNotificationSystem = NotificationSystem (return []) (return ()) (\_ -> return ())
