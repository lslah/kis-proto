{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
module Kis.Kis
    ( withKis
    , KisAction(..)
    , KisClient
    , Kis(..)
    )
where

import Control.Monad.Logger
import Control.Monad.RWS
import Control.Monad.Trans.Reader
import Database.Persist
import Data.Text

import Kis.Model

data KisAction a where
    CreateBed :: String -> KisAction BedId
    CreatePatient :: String -> KisAction PatientId
    GetPatient :: PatientId -> KisAction (Maybe Patient)
    GetPatients :: KisAction [Entity Patient]
    PlacePatient :: PatientId -> BedId -> KisAction (Maybe PatientBedId)

instance Show (KisAction a) where
    show (CreateBed name) = "Create bed " <> name
    show (CreatePatient name) = "Create patient " <> name
    show (GetPatient pid) = "Get patient " <> show pid
    show GetPatients = "Get patients"
    show (PlacePatient pid bid) = "Place patient " <> show pid <> " in bed " <> show bid

data Kis m = Kis { request :: forall a. KisAction a -> KisClient m a }
type KisClient m a = ReaderT (Kis m) m a

-- withProductionKis - uses Postgresql, Logging, etc.

withKis :: Kis m -> KisClient m () -> m ()
withKis kis action = runReaderT action kis

_logShow :: (MonadLogger m, Show a) => Text -> a -> m ()
_logShow tag x = logInfoN (tag <> ": " <> (pack . show $ x))
