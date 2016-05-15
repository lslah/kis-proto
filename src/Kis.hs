module Kis
    ( KisAction(..)
    , KisConfig(..)
    , KisClient
    , Kis(..)
    , waitForKisTime
    , withInMemoryKis
    , withKis
    , req
    , module Kis.Model
    )
where

import Kis.Kis
import Kis.Model
import Kis.Time
import Kis.SqlBackend

import Control.Monad.RWS

req :: Monad m => KisAction a -> KisClient m a
req action = do
    reqH <- asks k_requestHandler
    lift $ reqH action

waitForKisTime :: MonadIO m => TimeOffset -> KisClient m ()
waitForKisTime time =
    do clock <- asks k_clock
       let waitFor = c_waitFor clock
       liftIO $ waitFor time
