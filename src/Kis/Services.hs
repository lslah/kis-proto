{-# LANGUAGE RankNTypes #-}
module Kis.Services
    ( Service(..)
    , notificationsThread
    )
where

import Control.Monad
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
    -- These should live in KisReadOnlyClient, whenever that is implemented
    -> IO ()
notificationsThread kis handlers =
    forever $
        do waitForNextNotif
           notifications <- getNotifications
           forM notifications $
                \(Entity notifId notif) ->
                    do void $ forM handlers $ \h -> runClient kis $ h notif
                       deleteNotification notifId
        where
          waitForNextNotif = k_waitForNewNotification kis
          getNotifications = k_getNotifications kis
          deleteNotification = k_deleteNotification kis
