{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Kis.Model
where

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
    UniquePatientId patientId
    UniqueBedId bedId
    deriving Show
|]

