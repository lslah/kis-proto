{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Kis.Model
where

import Kis.Types
import Kis.Notifications
import Data.Time.Clock
import Database.Persist.TH
import Database.Persist.Sql as S
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


instance KisConversion Notification (Entity PNotification) where
  toKis (Entity idx pnot) =
        Notification
        { n_id = toKis idx
        , n_timestamp = pNotificationTimestamp pnot
        , n_payload = pNotificationPayload pnot
        , n_handlerSignature = pNotificationHandlerSig pnot
        }
  fromKis n = Entity (fromKis . n_id $ n) notification
      where notification =
              PNotification
              { pNotificationTimestamp = n_timestamp n
              , pNotificationPayload = n_payload n
              , pNotificationHandlerSig = n_handlerSignature n
              }

instance KisConversion Patient (Entity PPatient) where
  toKis (Entity idx ppat) = Patient { p_id = toKis idx, p_name = pPatientName ppat }
  fromKis p = Entity (fromKis . p_id $ p) pPat
      where pPat = PPatient { pPatientName = p_name p }

instance KisConversion PatientId PPatientId where
  toKis = PatientId . keyToInteger
  fromKis (PatientId idx) = integerToKey idx

instance KisConversion BedId PBedId where
  toKis = BedId . keyToInteger
  fromKis (BedId idx) = integerToKey idx

instance KisConversion NotificationId PNotificationId where
  toKis = NotificationId . keyToInteger
  fromKis (NotificationId idx) = integerToKey idx

integerToKey :: ToBackendKey SqlBackend a => Integer -> Key a
integerToKey = toSqlKey . fromIntegral

keyToInteger :: ToBackendKey SqlBackend a => Key a -> Integer
keyToInteger = fromIntegral . fromSqlKey
