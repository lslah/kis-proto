{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Kis.SqlBackend
    ( buildKisWithBackend
    , runClient
    , sqlMigrate
    , ExceptionMap
    , KisBackend(..)
    , RunNotifications(..)
    )
where

import Kis.Kis
import Kis.Notifications
import Kis.Model
import Kis.Types

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception hiding (handle)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Database.Persist.Sql as S
import Data.Maybe
import Data.Time.Clock
import qualified Database.Esqueleto as E
import qualified Data.ByteString as BS
import qualified Data.Text as T

-- | Every sql backend should provide a function of type 'ExceptionMap e'
-- which converts the backend specific exceptions (e.g. SqliteException)
-- to 'KisException's.
type ExceptionMap e = e -> KisException

data KisBackend e =
    SingleBackend SqlBackend (ExceptionMap e)
    | PoolBackend ConnectionPool (ExceptionMap e)

data RunNotifications = NoNotifs | RunNotifs (MVar ()) [NotificationHandler]

buildKisWithBackend ::
    (MonadCatch m, MonadBaseControl IO m, MonadIO m, Exception e)
    => KisBackend e
    -> KisConfig m
    -> RunNotifications
    -> IO (Kis m, Async ())
buildKisWithBackend backend (KisConfig clock) runNotifs =
    do wakeUpNotifThread <- newEmptyMVar
       lockDB <- newMVar ()
       (notifThread, notifHandlers) <-
           case runNotifs of
             NoNotifs ->
                 do emptyAsync <- async $ return ()
                    return (emptyAsync, [])
             RunNotifs stopSignal notifHandlers ->
                 do notifThread <-
                        async $
                            notificationsThread
                              (getNotifs lockDB)
                              (deleteNotif lockDB)
                              notifHandlers
                              wakeUpNotifThread
                              stopSignal
                    return (notifThread, notifHandlers)
       let kis = Kis { k_requestHandler =
                           \timestamp req ->
                               withLock lockDB $
                                   do res <- handleKisRequest backend notifHandlers timestamp req
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
    -> UTCTime
    -> forall a. KisRequest a
    -> m a
handleKisRequest backend notifHandlers timestamp request =
    runSql backend (handleRequest request)
    where
      handleRequest req =
          do res <- runAction req
             when (isWriteAction req) (saveNotificationActions req res)
             return res
      saveNotificationActions req res =
          mapM_ (\nh -> saveNotification nh res req) notifHandlers
      saveNotification nh res req =
          do mNotif <- nh_saveNotif nh (req, res)
             maybe (return ()) (writeNotif timestamp (nh_signature nh)) mNotif

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
runAction (CreateBed (BedSubmit name)) = fmap (BedId . fromSqlKey') $ S.insert (PBed name)
runAction (CreatePatient (PatientSubmit patient)) = fmap (PatientId . fromSqlKey') $ S.insert (PPatient patient)
runAction (GetPatient pid) = getPatientSql pid
runAction GetPatients = getPatientsSql
runAction (PlacePatient (PatientId patId) (BedId bedId)) =
    liftM isJust $ S.insertUnique (PPatientBed (toSqlKey' patId) (toSqlKey' bedId))

getPatientsSql :: MonadIO m => ReaderT SqlBackend m [Patient]
getPatientsSql =
    do patEntities <- E.select $ E.from $ \p -> return p
       return $ map pPatientToPatient patEntities

getPatientSql :: MonadIO m => PatientId -> ReaderT SqlBackend m (Maybe Patient)
getPatientSql (PatientId idx) =
    do mPat <- S.get patSqlKey
       return $ fmap (pPatientToPatient' patSqlKey) mPat
    where
      patSqlKey = toSqlKey' idx

pPatientToPatient :: Entity PPatient -> Patient
pPatientToPatient (Entity idx ppat) = pPatientToPatient' idx ppat

pPatientToPatient' :: PPatientId -> PPatient -> Patient
pPatientToPatient' ppatId (PPatient name) =
    Patient (PatientId (fromSqlKey' ppatId)) name

pNotifToNotif :: Entity PNotification -> Notification
pNotifToNotif (Entity idx pnot) = pNotifToNotif' idx pnot

pNotifToNotif' :: PNotificationId -> PNotification -> Notification
pNotifToNotif' pNotifId (PNotification timestamp payload handlerSig) =
    Notification
    { n_id = NotificationId $ fromSqlKey' pNotifId
    , n_timestamp = timestamp
    , n_payload = payload
    , n_handlerSignature = handlerSig
    }

getBackendNotifs ::
    (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m)
    => KisBackend e
    -> m [Notification]
getBackendNotifs backend =
    do pNotifs <- runSql backend getNotifQuery
       return $ map pNotifToNotif pNotifs
    where
      getNotifQuery :: MonadIO m => ReaderT SqlBackend m [Entity PNotification]
      getNotifQuery = E.select $ E.from $ \p -> return p

deleteNotificationInBackend ::
    (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m)
    => KisBackend e
    -> NotificationId
    -> m ()
deleteNotificationInBackend backend (NotificationId notifId) =
    runSql backend (delete (toSqlKey' notifId :: Key PNotification))

toSqlKey' :: ToBackendKey SqlBackend a => Integer -> Key a
toSqlKey' = toSqlKey . fromIntegral

fromSqlKey' :: ToBackendKey SqlBackend a => Key a -> Integer
fromSqlKey' = fromIntegral . fromSqlKey

writeNotif ::
    (MonadCatch m, MonadIO m)
    => UTCTime
    -> T.Text
    -> BS.ByteString
    -> ReaderT SqlBackend m ()
writeNotif timestamp signature payload =
    void $ S.insert (PNotification timestamp payload signature)

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

instance MonadIO m => KisRead (ReaderT SqlBackend m) where
    getPatients = getPatientsSql
    getPatient = getPatientSql
