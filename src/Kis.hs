module Kis
    ( KisRequest(..)
    , KisConfig(..)
    , KisClient
    , Kis(..)
    , KisException(..)
    , NotificationHandler(..)
    , SqliteBackendType(..)
    , WriteNotifFunc
    , realTimeClock
    , req
    , runClient
    , runSingleClientSqlite
    , runKis
    , waitForKisTime
    , withSqliteKis
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

req :: Monad m => KisRequest a -> KisClient m a
req action = do
    reqH <- asks k_requestHandler
    lift $ reqH action

waitForKisTime :: MonadIO m => TimeOffset -> KisClient m ()
waitForKisTime time =
    do clock <- asks k_clock
       let waitFor = c_waitFor clock
       liftIO $ waitFor time
