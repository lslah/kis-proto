module Kis
    ( KisAction(..)
    , KisClient
    , Kis(..)
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
req action = asks request >>= ($ action)
