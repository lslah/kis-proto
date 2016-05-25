{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Kis.Kis
    ( KisRequest(..)
    , KisClient
    , KisConfig(..)
    , Kis(..)
    , KisException(..)
    , RequestType(..)
    , requestType
    , toNotif
    )
where

import Control.Monad.Logger
import Control.Monad.RWS hiding (asks)
import Control.Monad.Reader
import Database.Persist
import Data.Text
import GHC.Exception

import Kis.Model
import Kis.Notifications
import Kis.Time

data KisRequest a where
    CreateBed :: String -> KisRequest BedId
    CreatePatient :: Patient -> KisRequest PatientId
    GetPatient :: PatientId -> KisRequest (Maybe Patient)
    GetPatients :: KisRequest [Entity Patient]
    PlacePatient :: PatientId -> BedId -> KisRequest (Maybe PatientBedId)

deriving instance Show (KisRequest a)

data RequestType = ReadRequest | WriteRequest

requestType :: KisRequest a -> RequestType
requestType (CreateBed _) = WriteRequest
requestType (CreatePatient _) = WriteRequest
requestType (PlacePatient _ _) = WriteRequest
requestType (GetPatient _) = ReadRequest
requestType GetPatients = ReadRequest

toNotif :: KisRequest a -> Notification
toNotif (CreateBed _) = Notification NewBed
toNotif (CreatePatient _) = Notification NewPatient
toNotif (PlacePatient _ _) = Notification PatientMoved
toNotif _ = Notification NoNotif

data Kis m =
    Kis
    { k_requestHandler :: forall a. KisRequest a -> m a
      -- ^ Interface with the data
    , k_clock :: Clock
      -- ^ Interface with clock functions
    , k_getNotifications :: m [Entity Notification]
    , k_waitForNewNotification :: m ()
    , k_deleteNotification :: NotificationId -> m ()
      -- ^ Interface to the notifications system
    }

type KisClient m = ReaderT (Kis m) m

data KisConfig = KisConfig Clock

data KisException =
    ConstraintViolation
    | OtherError Text
    deriving (Show, Eq)

instance Exception KisException

_logShow :: (MonadLogger m, Show a) => Text -> a -> m ()
_logShow tag x = logInfoN (tag <> ": " <> (pack . show $ x))
