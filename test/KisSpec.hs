{-# LANGUAGE GADTs #-}
module KisSpec
    ( spec
    )
where

import Kis

import Control.Monad
import Control.Monad.IO.Class
import Database.Persist.Sqlite
import Test.Hspec

spec :: Spec
spec = do
    describe "withKis" $
        it "can be parametrized" $
            withKis (Kis kis) $
                void $ req (CreatePatient "Thomas")
    describe "withInMemoryKis" $
        it "can create a Patient" $
            withInMemoryKis $ do
                pid <- req (CreatePatient "Thomas")
                patient <- req (GetPatient pid)
                liftIO $ liftM patientName patient `shouldBe` (Just "Thomas")


kis :: KisAction a -> KisClient IO a
kis (CreatePatient _) = return (toSqlKey 1)
kis _ = undefined

