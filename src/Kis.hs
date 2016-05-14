module Kis
    ( KisAction(..)
    , KisConfig(..)
    , KisClient
    , Kis(..)
    , getKisTime
    , waitForKisTime
    , withInMemoryKis
    , withKis
    , req
    , module Kis.Model
    )
where

import Kis.Kis
import Kis.Model
import Kis.SqlBackend

import Control.Monad.RWS

req :: Monad m => KisAction a -> KisClient m a
req action = do
    reqH <- asks k_requestHandler
    lift $ reqH action
