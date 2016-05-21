module Kis
    ( KisRequest
    , KisWriteRequest(..)
    , KisReadRequest(..)
    , KisConfig(..)
    , KisClient
    , Kis(..)
    , KisException(..)
    , createPatient
    , createBed
    , placePatient
    , getPatient
    , getPatients
    , waitForKisTime
    , withSqliteKis
    , SqliteBackendType(..)
    , withKis
    , req
    , module Kis.Model
    )
where

import Kis.Kis
import Kis.Model
import Kis.Time
import Kis.SqliteBackend

import Control.Monad.RWS

req :: Monad m => KisRequest a -> KisClient m a
req action = do
    reqH <- asks k_requestHandler
    lift $ reqH action

waitForKisTime :: MonadIO m => TimeOffset -> KisClient m ()
waitForKisTime time =
    do clock <- asks k_clock
       let waitFor = c_waitFor clock
       liftIO $ waitFor time
