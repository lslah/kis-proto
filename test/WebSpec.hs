{-# LANGUAGE OverloadedStrings #-}
module WebSpec
    ( spec
    )
where

import Web

import Control.Monad
import Data.Aeson
import Data.Either.Extra
import Kis.Types
import Network.Wai.Test
import Test.Hspec hiding (pending)
import Test.Hspec.Wai

spec :: Spec
spec =
    with inMemoryApplication $
        describe "Fresh inMemoryApplication" $ do
            it "starts with empty list of patients" $
                get "/patients" `shouldDecodeTo` ([] :: [Patient])
            context "creating a patient" $ do
                let patient = PatientSubmit "Thomas"
                it "responds with new patient id" $
                    post "/patient/" (encode patient) `shouldDecodeTo` (1 :: Int)
                it "new patient occurs in patient list" $ do
                    patId <- decodeResponse $ post "/patient/" (encode patient)
                    get "/patients" `shouldDecodeTo` [Patient patId "Thomas"]

decodeResponse :: FromJSON a => WaiSession SResponse -> WaiSession a
decodeResponse req =
    liftM (fromRight . eitherDecode . simpleBody) req

shouldDecodeTo :: (Eq b, Show b, ToJSON b) => WaiSession SResponse -> b -> WaiExpectation
shouldDecodeTo a b =
    a `shouldRespondWith` ResponseMatcher 200 [] (Just $ encode b)
