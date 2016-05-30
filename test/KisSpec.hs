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
import System.IO.Temp
import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "runClient" $
        it "can be parametrized" $
            runClient customKis $
                void $ req (CreatePatient $ Patient "Thomas")
    describe "runSingleClientSqlite" $ do
        it "can create a Patient" $
            runSingleClientSqlite InMemory kisConfig $ do
                pid <- req (CreatePatient $ Patient "Thomas")
                patient <- req (GetPatient pid)
                liftIO $ liftM patientName patient `shouldBe` (Just "Thomas")
        it "can place patient in bed" $
            runSingleClientSqlite InMemory kisConfig $ do
                pat <- req (CreatePatient $ Patient "Thomas")
                bed <- req (CreateBed "xy")
                patBed <- req (PlacePatient pat bed)
                liftIO $ patBed `shouldSatisfy` isJust
        it "can't place nonexisting patient in bed" $
            (runSingleClientSqlite InMemory kisConfig $ do
                bed <- req (CreateBed "xy")
                void $ req (PlacePatient (toSqlKey 1) bed))
            `shouldThrow` (== ConstraintViolation)
        it "can't place patient in nonexisting bed" $
            (runSingleClientSqlite InMemory kisConfig $ do
                pat <- req (CreatePatient $ Patient "xy")
                void $ req (PlacePatient pat (toSqlKey 1)))
            `shouldThrow` (== ConstraintViolation)
    describe "withSqliteKis" $
         it "can run two clients in sequence" $
            withSqliteKis InMemory kisConfig $ \kis -> do
                patId <- runClient kis $ req (CreatePatient $ Patient "Simon")
                pats <- runClient kis $ req GetPatients
                pats `shouldBe` [Entity patId (Patient "Simon")]
    describe "withSqlitePool" $ do
          it "can run two clients in sequence" $
             withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
                 withSqliteKis (PoolBackendType (T.pack fp) 10) kisConfig $ \kis ->
                     do patId <- runClient kis $ req (CreatePatient $ Patient "Simon")
                        pats <- runClient kis $ req GetPatients
                        liftIO $ pats `shouldBe` [Entity patId (Patient "Simon")]
          it "can run two clients in parallel" $
             withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
                 withSqliteKis (PoolBackendType (T.pack fp) 10) kisConfig $ \kis ->
                     do firstClientDone <- newEmptyMVar
                        let client1 =
                                do void $ req (CreatePatient $ Patient "Simon")
                                   liftIO $ putMVar firstClientDone ()
                            client2 =
                                do void $ req (CreatePatient $ Patient "Thomas")
                                   liftIO $ takeMVar firstClientDone
                                   pats <- req GetPatients
                                   liftIO $ map (\(Entity _ pat) -> pat) pats `shouldBe` [Patient "Simon"]
                        link =<< async (runClient kis client1)
                        link =<< async (runClient kis client2)

customKisFunction :: KisRequest a -> IO a
customKisFunction (CreatePatient _) = return (toSqlKey 1)
customKisFunction _ = undefined

customKis :: Kis IO
customKis =
    Kis customKisFunction realTimeClock mockNotificationSystem

kisConfig :: KisConfig
kisConfig = KisConfig realTimeClock

mockNotificationSystem :: NotificationSystem IO
mockNotificationSystem = NotificationSystem (return []) (return ()) (\_ -> return ())
