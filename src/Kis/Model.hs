{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Kis.Model
where

import Database.Persist.TH
import qualified Data.ByteString as BS
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Patient json
    name String
    deriving Show Eq
Bed
    name String
    deriving Show Eq

PatientBed
    patientId PatientId
    bedId BedId
    UniquePatientId patientId
    UniqueBedId bedId
    deriving Show Eq

Notification
    payload BS.ByteString
    handlerSig T.Text
    deriving Show Eq
|]

