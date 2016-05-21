{-# LANGUAGE RankNTypes #-}

module Kis.ReadOnly
    ( runReadOnlyClient
    , KisReadOnlyClient
    )
where

import Control.Monad.Reader

import Kis.Kis
import Kis.Time

data ReadOnlyKis m =
    ReadOnlyKis
    { _rok_requestHandler :: forall a. KisReadRequest a -> m a
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
      roRequestHandler action = reqHandler (Right action)
