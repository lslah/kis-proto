{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Kis.Kis
    ( withKis
    , KisAction(..)
    , KisClient
    , KisConfig(..)
    , Kis(..)
    )
where

import Control.Monad.Logger
import Control.Monad.RWS hiding (asks)
import Control.Monad.Trans.Reader
import Database.Persist
import Data.Text

import Kis.Model
import Kis.Time

data KisAction a where
    CreateBed :: String -> KisAction BedId
    CreatePatient :: Patient -> KisAction PatientId
    GetPatient :: PatientId -> KisAction (Maybe Patient)
    GetPatients :: KisAction [Entity Patient]
    PlacePatient :: PatientId -> BedId -> KisAction (Maybe PatientBedId)

deriving instance Show (KisAction a)

data Kis m =
    Kis
    { k_requestHandler :: forall a. KisAction a -> m a
    , k_clock :: Clock
    }
type KisClient m = ReaderT (Kis m) m

data KisConfig = KisConfig Clock

-- withProductionKis - uses Postgresql, Logging, etc.

withKis :: Kis m -> KisClient m a -> m a
withKis kis action = runReaderT action kis

_logShow :: (MonadLogger m, Show a) => Text -> a -> m ()
_logShow tag x = logInfoN (tag <> ": " <> (pack . show $ x))
