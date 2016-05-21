{-# LANGUAGE RankNTypes #-}

module Kis.ReadOnly
    ( req
    , runReadOnlyClient
    , KisReadOnlyClient
    )
where

import Control.Monad.Reader

import Kis.Kis
import Kis.Time

data ReadOnlyKis m =
    ReadOnlyKis
    { rok_requestHandler :: forall a. KisReadRequest a -> m a
    , _rok_clock :: Clock
    }

type KisReadOnlyClient m = ReaderT (ReadOnlyKis m) m

runReadOnlyClient :: Kis m -> KisReadOnlyClient m a -> m a
runReadOnlyClient kis readOnlyClient =
    runReaderT readOnlyClient (toReadOnlyKis kis)

toReadOnlyKis :: Kis m -> ReadOnlyKis m
toReadOnlyKis (Kis reqHandler clock) =
    ReadOnlyKis roRequestHandler clock
    where
      roRequestHandler request = reqHandler (Right request)

req :: Monad m => KisReadRequest a -> KisReadOnlyClient m a
req request =
    do reqH <- asks rok_requestHandler
       lift $ reqH request
