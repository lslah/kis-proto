{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Patient
    name String
    deriving Show
Bed
    name String
    deriving Show

PatientBed
    patientId PatientId
    bedId BedId
    deriving Show
|]

placePatient patient bed = undefined

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    thomasId <- insert $ Patient "Thomas Wienecke"
    piaId <- insert $ Patient "Pia von Rosenberg"

    bedXY <- insert $ Bed "BedXY"
    bedYZ <- insert $ Bed "BedYZ"

    patientBedId <- insert $ PatientBed thomasId bedXY

    patientBed <- get patientBedId
    liftIO $ print patientBed
