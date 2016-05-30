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
    , NotificationSystem(..)
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

toNotif :: KisRequest a -> Maybe Notification
toNotif (CreateBed _) = Just $ Notification NewBed
toNotif (CreatePatient _) = Just $ Notification NewPatient
toNotif (PlacePatient _ _) = Just $ Notification PatientMoved
toNotif _ = Nothing

data Kis m =
    Kis
    { k_requestHandler :: forall a. KisRequest a -> m a
      -- ^ Interface with the data
    , k_clock :: Clock
      -- ^ Interface with clock functions
    , k_notificationSystem :: NotificationSystem m
    }

data NotificationSystem m =
    NotificationSystem
    { ns_getNotifications :: m [Entity Notification]
    , ns_waitForNewNotification :: m ()
    , ns_deleteNotification :: NotificationId -> m ()
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
