module Kis
    ( KisRequest(..)
    , KisConfig(..)
    , KisClient
    , Kis(..)
    , KisException(..)
    , NotificationType(..)
    , NotificationSystem(..)
    , Service(..)
    , SqliteBackendType(..)
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
import Kis.Services
import Kis.Time
import Kis.SqliteBackend

import Control.Concurrent.Async
import Control.Monad.RWS
import qualified Data.Text as T

runKis :: [Service IO] -> T.Text -> IO ()
runKis services dbFile =
    withSqliteKis (PoolBackendType dbFile 10) (KisConfig realTimeClock) $ \kis ->
        do link =<< async (notificationsThread kis notificationHandlers)
           void $ forM_ services $ \s -> wait =<< async (runClient kis (s_serviceMain s))
    where
      notificationHandlers = map s_notificationHandler services

req :: Monad m => KisRequest a -> KisClient m a
req action = do
    reqH <- asks k_requestHandler
    lift $ reqH action

waitForKisTime :: MonadIO m => TimeOffset -> KisClient m ()
waitForKisTime time =
    do clock <- asks k_clock
       let waitFor = c_waitFor clock
       liftIO $ waitFor time
