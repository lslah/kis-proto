{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module KisSpec
    ( spec
    )
where

import Kis
import Kis.Time

import Control.Monad
import Control.Monad.Except
import Database.Persist.Sqlite
import Data.Maybe
import Test.Hspec

spec :: Spec
spec = do
    describe "runClient" $
        it "can be parametrized" $
            runClient (Kis customKisFunction realTimeClock) $
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
    describe "withSqliteKis" $ do
         it "can run two clients in sequence" $
            withSqliteKis InMemory kisConfig $ \kis -> do
                patId <- runClient kis $ req (CreatePatient $ Patient "Simon")
                pats <- runClient kis $ req GetPatients
                pats `shouldBe` [Entity patId (Patient "Simon")]

customKisFunction :: KisRequest a -> IO a
customKisFunction (CreatePatient _) = return (toSqlKey 1)
customKisFunction _ = undefined

kisConfig :: KisConfig
kisConfig = KisConfig realTimeClock

