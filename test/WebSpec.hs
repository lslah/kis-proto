{-# LANGUAGE OverloadedStrings #-}
module WebSpec
    ( spec
    )
where

import Web

import Test.Hspec hiding (pending)
import Test.Hspec.Wai
import Data.Monoid
import Data.Aeson
import Control.Monad
import Network.Wai.Test
import Kis.Model
import Database.Persist hiding (get)

spec :: Spec
spec =
    with inMemoryApplication $
        describe "Fresh inMemoryApplication" $ do
            it "starts with empty list of patients" $
                get "/patients" `shouldDecodeTo` ([] :: [Entity Patient])
            it "can create a patient" $ do
                let name = "Thomas"
                patId <- shouldDecode $ post ("/patient/" <> name) ""
                get "/patients" `shouldDecodeTo` [Entity patId (Patient "Thomas")]

shouldDecode :: FromJSON a => WaiSession SResponse -> WaiSession a
shouldDecode a = do
    x <- liftM (eitherDecode . simpleBody) a
    either error return x

shouldDecodeTo :: (Eq b, Show b, FromJSON b) => WaiSession SResponse -> b -> WaiExpectation
shouldDecodeTo a b = do
    x <- shouldDecode a
    liftIO $ x `shouldBe` b
