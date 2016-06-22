{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

module Kis.Kis
    ( KisRequest(..)
    , KisClient
    , KisConfig(..)
    , Kis(..)
    , KisException(..)
    , KisClock(..)
    , KisRead(..)
    , KisWrite(..)
    , isWriteAction
    )
where

import Control.Monad.Logger
import Control.Monad.RWS hiding (asks)
import Control.Monad.Reader
import Database.Persist
import Data.Text
import Data.Time.Clock
import GHC.Exception

import Kis.Model
import Kis.Time

data Kis m =
    Kis
    { k_requestHandler :: forall a. KisRequest a -> m a
      -- ^ Interface with the data
    , k_clock :: Clock
      -- ^ Interface with clock functions
    }

data KisRequest a where
    CreateBed :: String -> KisRequest BedId
    CreatePatient :: Patient -> KisRequest PatientId
    GetPatient :: PatientId -> KisRequest (Maybe Patient)
    GetPatients :: KisRequest [Entity Patient]
    PlacePatient :: PatientId -> BedId -> KisRequest (Maybe PatientBedId)

deriving instance Show (KisRequest a)

isWriteAction :: KisRequest a -> Bool
isWriteAction (CreateBed _) = True
isWriteAction (CreatePatient _) = True
isWriteAction (PlacePatient _ _) = True
isWriteAction _ = False

class KisRead m where
    getPatients ::  m [Entity Patient]
    getPatient :: PatientId -> m (Maybe Patient)

class KisWrite m where
    createBed :: String -> m BedId
    createPatient :: Patient -> m PatientId
    placePatient :: PatientId -> BedId -> m (Maybe PatientBedId)

class KisClock m where
    getKisTime :: m UTCTime
    waitForKisTime :: TimeOffset -> m ()

type KisClient m = ReaderT (Kis m) m

instance Monad m => KisRead (KisClient m) where
    getPatients = req GetPatients
    getPatient patId = req (GetPatient patId)

instance Monad m => KisWrite (KisClient m) where
    createBed str = req (CreateBed str)
    createPatient pat = req (CreatePatient pat)
    placePatient patId bedId = req (PlacePatient patId bedId)

instance MonadIO m => KisClock (KisClient m) where
    getKisTime =
        do clock <- asks k_clock
           liftIO $ c_getTime clock
    waitForKisTime time =
        do clock <- asks k_clock
           let waitFor = c_waitFor clock
           liftIO $ waitFor time

data KisConfig = KisConfig Clock

data KisException =
    ConstraintViolation
    | OtherError Text
    deriving (Show, Eq)

instance Exception KisException

_logShow :: (MonadLogger m, Show a) => Text -> a -> m ()
_logShow tag x = logInfoN (tag <> ": " <> (pack . show $ x))

req :: Monad m => KisRequest a -> KisClient m a
req action = do
    reqH <- asks k_requestHandler
    lift $ reqH action
