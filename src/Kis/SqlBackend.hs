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

import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Database.Persist.Sql as S
import qualified Database.Esqueleto as E

import Kis.Model
import Kis.Kis

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
    -> IO (Kis m)
buildKisWithBackend backend (KisConfig clock) =
    do wakeUp <- newEmptyMVar
       return $ Kis { k_requestHandler =
                          \req ->
                              do res <- handleKisRequest backend req
                                 notify wakeUp req
                                 return res
                    , k_clock = clock
                    , k_notificationSystem =
                        NotificationSystem
                        { ns_getNotifications = getBackendNotifs backend
                        , ns_waitForNewNotification = liftIO $ takeMVar wakeUp
                        , ns_deleteNotification = deleteNotificationInBackend backend
                        }
                    }
    where
      notify wakeUp request =
            whenJust (toNotif request) (\_ -> void $ liftIO $ tryPutMVar wakeUp ())

runClient :: Kis m -> KisClient m a -> m a
runClient kis client = runReaderT client kis

handleKisRequest :: (Exception e, MonadCatch m, MonadBaseControl IO m, MonadIO m) =>
    KisBackend e -> forall a. KisRequest a -> m a
handleKisRequest backend req =
    runSql backend handleRequest
    where
      handleRequest =
          do res <- runAction req
             writeNotif req
             return res
      writeNotif request =
          whenJust (toNotif request) (void . S.insert)

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
