{-# LANGUAGE RankNTypes #-}
module Kis.Services
    ( Service(..)
    , notificationsThread
    )
where

import Control.Monad

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
                \n -> forM handlers $
                    -- | When ReadOnly is implemented, this should be: runReadOnlyClient
                    \h -> runClient kis $ h n
        where
          waitForNextNotif = k_waitForNewNotification kis
          getNotifications = k_getNotifications kis
