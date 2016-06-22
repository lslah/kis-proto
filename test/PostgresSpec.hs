{-# LANGUAGE OverloadedStrings #-}

module PostgresSpec
  ( spec
  )
where

import Test.Hspec
import Database.PostgreSQL.Simple

_CONNECTION_INFO_ :: ConnectInfo
_CONNECTION_INFO_ =
    defaultConnectInfo
    { connectDatabase = "kis"
    , connectPassword = "postgres"
    }

spec :: Spec
spec =
  do describe "postgresql" $
       it "connects" $
       do conn <- connect _CONNECTION_INFO_
          [Only i] <- query_ conn "SELECT 2 + 2"
          i `shouldBe` (4 :: Int)
          close conn
     -- describe "postgres-backend" $
     --   it "creates beds" $
     --       withPostgresKis _CONNECTION_INFO_ $
     --       do idx <- createBed "xyz"
     --          bed <- getBed idx
     --          bed `shouldBe` Just (Bed idx "xyz")
