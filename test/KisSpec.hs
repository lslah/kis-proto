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
    describe "withKis" $
        it "can be parametrized" $
            withKis (Kis kis realTimeClock) $
                void $ req (createPatient $ Patient "Thomas")
    describe "withInMemoryKis" $ do
        it "can create a Patient" $
            withSqliteKis InMemory kisConfig $ do
                pid <- req (createPatient $ Patient "Thomas")
                patient <- req (getPatient pid)
                liftIO $ liftM patientName patient `shouldBe` (Just "Thomas")
        it "can place patient in bed" $
            withSqliteKis InMemory kisConfig $ do
                pat <- req (createPatient $ Patient "Thomas")
                bed <- req (createBed "xy")
                patBed <- req (placePatient pat bed)
                liftIO $ patBed `shouldSatisfy` isJust
        -- Are the next two statements necessary? Should we always aim for a
        -- consistent database or deal with inconsistencies like
        -- patient-bed-relations of nonexisting entities? What happens when a
        -- patient is assigned to a deleted bed?
        it "can't place nonexisting patient in bed" $
            (withSqliteKis InMemory kisConfig $ do
                bed <- req (createBed "xy")
                void $ req (placePatient (toSqlKey 1) bed))
            `shouldThrow` (== ConstraintViolation)
        it "can't place patient in nonexisting bed" $
            (withSqliteKis InMemory kisConfig $ do
                pat <- req (createPatient $ Patient "xy")
                void $ req (placePatient pat (toSqlKey 1)))
            `shouldThrow` (== ConstraintViolation)

kis :: KisRequest a -> IO a
kis (Left (CreatePatient _)) = return (toSqlKey 1)
kis _ = undefined

kisConfig :: KisConfig
kisConfig = KisConfig realTimeClock

