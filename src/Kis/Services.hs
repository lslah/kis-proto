{-# LANGUAGE RankNTypes #-}
module Kis.Services
    ( Service(..)
    , notificationsThread
    )
where

import Control.Concurrent
import Control.Monad
import Data.Maybe (isJust)
import Database.Persist

import Kis.Kis
import Kis.Model
import Kis.SqliteBackend

data Service m =
    Service
    { s_serviceMain :: KisClient m ()
    , s_notificationHandler :: Notification -> KisClient m ()
    }

notificationsThread ::
    Kis IO
    -> [Notification -> KisClient IO ()]
    -- ^ Notificationhandlers that are run whenever a new notification is written.
    -> MVar ()
    -> IO ()
notificationsThread kis handlers stopSignal =
    loop
    where
      loop =
          do shouldStop <- tryReadMVar stopSignal
             case isJust shouldStop of
               True ->
                   do notifs <- getNotifications
                      case notifs of
                        [] -> return ()
                        _ -> handleNotifications
               False -> handleNotifications
      handleNotifications =
          do waitForNextNotif
             notifications <- getNotifications
             forM_ notifications $
                 \(Entity notifId notif) ->
                     do void $ forM handlers $ \h -> runClient kis $ h notif
                        deleteNotification notifId
             loop
      waitForNextNotif = ns_waitForNewNotification notificationSystem
      getNotifications = ns_getNotifications notificationSystem
      deleteNotification = ns_deleteNotification notificationSystem
      notificationSystem = k_notificationSystem kis
