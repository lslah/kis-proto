{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
module Kis.SqlBackend
    ( buildKisWithBackend
    , runClient
    , sqlMigrate
    , ExceptionMap
    , KisBackend(..)
    )
where

import Kis.Kis
import Kis.Notifications
import Kis.Model
import Kis.Time

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception hiding (handle)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Database.Persist.Sql as S
import qualified Data.ByteString as BS
import qualified Database.Esqueleto as E
import qualified Data.Text as T

-- | Every sql backend should provide a function of type 'ExceptionMap e'
-- which converts the backend specific exceptions (e.g. SqliteException)
-- to 'KisException's.
type ExceptionMap e = e -> KisException

data KisBackend e =
    SingleBackend SqlBackend (ExceptionMap e)
    | PoolBackend ConnectionPool (ExceptionMap e)

buildKisWithBackend ::
    (MonadCatch m, MonadBaseControl IO m, MonadIO m, Exception e)
    => KisBackend e
    -> KisConfig
    -> [NotificationHandler]
    -> MVar ()
    -> IO (Kis m, Async ())
buildKisWithBackend backend (KisConfig clock) notifHandlers stopSignal =
    do wakeUpNotifThread <- newEmptyMVar
       lockDB <- newMVar ()
       notifThread <-
           async $
               notificationsThread
                 (getNotifs lockDB)
                 (deleteNotif lockDB)
                 notifHandlers
                 wakeUpNotifThread
                 stopSignal
       let kis = Kis { k_requestHandler =
                           \req ->
                               withLock lockDB $
                                   do res <- handleKisRequest backend notifHandlers clock req
                                      signalNotifThread wakeUpNotifThread req
                                      return res
                     , k_clock = clock
                     }
       return (kis, notifThread)
    where
      signalNotifThread wakeUpNotifThread request =
          when (isWriteAction request) (void $ liftIO $ tryPutMVar wakeUpNotifThread ())
      getNotifs lock = withLock lock $ getBackendNotifs backend
      deleteNotif lock = \nid -> withLock lock $ deleteNotificationInBackend backend nid

runClient :: Kis m -> KisClient m a -> m a
runClient kis client = runReaderT client kis

handleKisRequest ::
    (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m)
    => KisBackend e
    -> [NotificationHandler]
    -> Clock
    -> forall a. KisRequest a
    -> m a
handleKisRequest backend notifHandlers clock request =
    runSql backend (handleRequest request)
    where
      handleRequest req =
          do res <- runAction req
             case isWriteAction req of
               True -> saveNotificationActions req res
               False -> return ()
             return res
      saveNotificationActions req res =
          mapM_ (\nh -> runClient kis (saveNotification nh res req)) notifHandlers
      saveNotification nh res req =
          (nh_saveNotif nh) (nh_signature nh) (req, res) (void . writeNotif)
      kis = Kis handleSqlRequest clock
      handleSqlRequest req =
          case isWriteAction req of
            True -> error "Save-Notification action spawned a write request"
            False ->
                do res <- runAction req
                   return res

convertException :: (Exception e, MonadCatch m) => (e -> KisException) -> m a -> m a
convertException exceptionMap = handle (throwM . exceptionMap)

runSql :: (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m)
       => KisBackend e -> SqlPersistT m a -> m a
runSql (SingleBackend backend exceptionMap) query =
    convertException exceptionMap (runSqlConn query backend)
runSql (PoolBackend pool exceptionMap) query =
    convertException exceptionMap (runSqlPool query pool)

sqlMigrate :: (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m)
       => KisBackend e -> m ()
sqlMigrate backend = void $ runSql backend (runMigrationSilent migrateAll)

runAction :: (MonadCatch m, MonadIO m)
          => KisRequest a -> ReaderT SqlBackend m a
runAction (CreateBed name) = S.insert (Bed name)
runAction (CreatePatient patient) = S.insert patient
runAction (GetPatient pid) = S.get pid
runAction GetPatients = getPatients
runAction (PlacePatient patId bedId) = S.insertUnique (PatientBed patId bedId)

getPatients :: MonadIO m => ReaderT SqlBackend m [Entity Patient]
getPatients = E.select $ E.from $ \p -> return p

getBackendNotifs ::
    (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m)
    => KisBackend e
    -> m [Entity Notification]
getBackendNotifs backend = runSql backend getNotifQuery
    where
      getNotifQuery :: MonadIO m => ReaderT SqlBackend m [Entity Notification]
      getNotifQuery = E.select $ E.from $ \p -> return p

deleteNotificationInBackend ::
    (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m)
    => KisBackend e
    -> NotificationId
    -> m ()
deleteNotificationInBackend backend notifId = runSql backend (delete notifId)

writeNotif ::
    (MonadCatch m, MonadIO m)
    => (T.Text, BS.ByteString)
    -> ReaderT SqlBackend m NotificationId
writeNotif (signature, payload) = S.insert (Notification payload signature)

withLock ::
    (MonadCatch m, MonadBaseControl IO m, MonadIO m)
    => MVar ()
    -> m a
    -> m a
withLock lock f =
    do liftIO (takeMVar lock)
       res <- f
       liftIO (putMVar lock ())
       return res
