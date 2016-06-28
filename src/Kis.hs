module Kis
    ( KisRequest(..)
    , KisConfig(..)
    , KisClient
    , Kis(..)
    , KisException(..)
    , KisClock(..)
    , KisRead(..)
    , KisWrite(..)
    , NotificationHandler(..)
    , SqliteBackendType(..)
    , constClock
    , realTimeClock
    , runClient
    , runSingleClientSqlite
    , runKis
    , withSqliteKis
    , withSqliteKisWithNotifs
    , module Kis.Model
    )
where

import Kis.Kis
import Kis.Model
import Kis.Notifications
import Kis.Time
import Kis.SqliteBackend

import Control.Concurrent.Async
import Control.Monad.RWS
import qualified Data.Text as T

runKis :: [KisClient IO a] -> [NotificationHandler] -> T.Text -> IO ()
runKis clients notifHandlers dbFile =
    withSqliteKisWithNotifs poolbackend kisConfig notifHandlers $ \kis ->
        do allClients <- forM clients $ \client -> async (runClient kis client)
           mapM_ wait allClients
    where
      poolbackend = PoolBackendType dbFile 10
      kisConfig = KisConfig realTimeClock

