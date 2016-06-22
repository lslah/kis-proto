{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
import Control.Monad.Reader
import Control.Monad.RWS hiding (asks)
import Data.Text
import Data.Time.Clock
import GHC.Exception

import Kis.Time
import Kis.Types

data Kis m =
    Kis
    { k_requestHandler :: forall a. UTCTime -> KisRequest a -> m a
      -- ^ Interface with the data
    , k_clock :: Clock m
      -- ^ Interface with clock functions
    }

data KisRequest a where
    CreateBed :: BedSubmit -> KisRequest BedId
    CreatePatient :: PatientSubmit -> KisRequest PatientId
    GetPatient :: PatientId -> KisRequest (Maybe Patient)
    GetPatients :: KisRequest [Patient]
    PlacePatient :: PatientId -> BedId -> KisRequest Bool

deriving instance Show (KisRequest a)

isWriteAction :: KisRequest a -> Bool
isWriteAction (CreateBed _) = True
isWriteAction (CreatePatient _) = True
isWriteAction (PlacePatient _ _) = True
isWriteAction _ = False

class KisRead m where
    getPatients ::  m [Patient]
    getPatient :: PatientId -> m (Maybe Patient)

class KisWrite m where
    createBed :: BedSubmit -> m BedId
    createPatient :: PatientSubmit -> m PatientId
    placePatient :: PatientId -> BedId -> m Bool

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

instance Monad m => KisClock (KisClient m) where
    getKisTime =
        do clock <- asks k_clock
           lift $ c_getTime clock
    waitForKisTime time =
        do clock <- asks k_clock
           let waitFor = c_waitFor clock
           lift $ waitFor time

data KisConfig m = KisConfig (Clock m)

data KisException =
    ConstraintViolation
    | OtherError Text
    deriving (Show, Eq)

instance Exception KisException

_logShow :: (MonadLogger m, Show a) => Text -> a -> m ()
_logShow tag x = logInfoN (tag <> ": " <> (pack . show $ x))

req :: Monad m => KisRequest a -> KisClient m a
req action = do
    timestamp <- getKisTime
    reqH <- asks k_requestHandler
    lift $ reqH timestamp action
