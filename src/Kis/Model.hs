{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Kis.Model
where

import Data.Time.Clock
import Database.Persist.TH
import qualified Data.ByteString as BS
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
PPatient json
    name String
    deriving Show Eq
PBed
    name String
    deriving Show Eq

PPatientBed
    patientId PPatientId
    bedId PBedId
    UniquePatientId patientId
    UniqueBedId bedId
    deriving Show Eq

PNotification
    timestamp UTCTime
    payload BS.ByteString
    handlerSig T.Text
    deriving Show Eq
|]

