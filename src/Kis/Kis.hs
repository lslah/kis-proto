{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Kis.Kis
    ( withKis
    , createPatient
    , createBed
    , placePatient
    , getPatient
    , getPatients
    , KisRequest
    , KisWriteRequest(..)
    , KisReadRequest(..)
    , KisClient
    , KisConfig(..)
    , Kis(..)
    , KisException(..)
    )
where

import Control.Monad.Logger
import Control.Monad.RWS hiding (asks)
import Control.Monad.Reader
import Database.Persist
import Data.Text
import GHC.Exception

import Kis.Model
import Kis.Time

type KisRequest a = Either (KisWriteRequest a) (KisReadRequest a)

data KisWriteRequest a where
    CreateBed :: String -> KisWriteRequest BedId
    CreatePatient :: Patient -> KisWriteRequest PatientId
    PlacePatient :: PatientId -> BedId -> KisWriteRequest (Maybe PatientBedId)

deriving instance Show (KisWriteRequest a)

data KisReadRequest a where
    GetPatient :: PatientId -> KisReadRequest (Maybe Patient)
    GetPatients :: KisReadRequest [Entity Patient]

deriving instance Show (KisReadRequest a)

data Kis m =
    Kis
    { k_requestHandler :: forall a. KisRequest a -> m a
    , k_clock :: Clock
    }
type KisClient m = ReaderT (Kis m) m

data KisConfig = KisConfig Clock

data KisException =
    ConstraintViolation
    | OtherError Text
    deriving (Show, Eq)

instance Exception KisException

createPatient :: Patient -> KisRequest PatientId
createPatient pat = Left (CreatePatient pat)

createBed :: String -> KisRequest BedId
createBed name = Left (CreateBed name)

placePatient :: PatientId -> BedId -> KisRequest (Maybe PatientBedId)
placePatient patId bedId = Left (PlacePatient patId bedId)

getPatient :: PatientId -> KisRequest (Maybe Patient)
getPatient patId = Right (GetPatient patId)

getPatients :: KisRequest [Entity Patient]
getPatients = Right GetPatients

withKis :: Monad m => Kis m -> KisClient m a -> m a
withKis kis action = runReaderT action kis

_logShow :: (MonadLogger m, Show a) => Text -> a -> m ()
_logShow tag x = logInfoN (tag <> ": " <> (pack . show $ x))
