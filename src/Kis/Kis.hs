{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Kis.Kis
    ( withKis
    , getKisTime
    , waitForKisTime
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
import Data.Time.Clock
import System.Time.Extra (sleep)

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
    , k_time :: KisTime
    }
type KisClient m = ReaderT (Kis m) m

data KisConfig = KisConfig KisTime

getKisTime :: (MonadIO m) => KisClient m UTCTime
getKisTime =
    do kisTime <- asks k_time
       case kisTime of
         RealTime -> liftIO getCurrentTime
         VirtualTime virtualTimeStart virtualTimeMult ->
             do now <- liftIO getCurrentTime
                let timeElapsed = diffUTCTime now virtualTimeStart
                    virtualDiffTime =
                        (timeElapsed * (toDiffTime virtualTimeMult))
                liftIO . return $ addUTCTime virtualDiffTime virtualTimeStart
  where
    toDiffTime = fromIntegral . toInteger

waitForKisTime :: (MonadIO m) => TimeOffset -> KisClient m ()
waitForKisTime (TimeOffset diffTime) =
    do kisTime <- asks k_time
       case kisTime of
         RealTime -> liftIO (sleep diffTimeInSeconds)
         VirtualTime _ mult ->
             liftIO . sleep $ diffTimeInSeconds / (fromIntegral mult :: Double)
    where
      diffTimeInSeconds = fromRational $ toRational diffTime :: Double

-- withProductionKis - uses Postgresql, Logging, etc.

withKis :: Kis m -> KisClient m a -> m a
withKis kis action = runReaderT action kis

_logShow :: (MonadLogger m, Show a) => Text -> a -> m ()
_logShow tag x = logInfoN (tag <> ": " <> (pack . show $ x))
