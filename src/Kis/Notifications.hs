{-# LANGUAGE RankNTypes #-}
module Kis.Notifications
    ( NotificationHandler(..)
    , notificationsThread
    )
where

import Kis.Kis
import Kis.Model


import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Maybe (isJust)
import Data.Time.Clock
import Database.Persist
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T

data NotificationHandler =
    NotificationHandler
    { nh_saveNotif ::
          forall a m . (KisRead m, Monad m)
          => (KisRequest a, a)
          -> m (Maybe BS.ByteString)
    , nh_processNotif :: UTCTime -> BS.ByteString -> IO ()
    , nh_signature :: T.Text
    }

notificationsThread ::
    IO [Entity Notification]
    -- ^ Function to retrieve Notifications
    -> (NotificationId -> IO ())
    -- ^ Function to delete Notifications
    -> [NotificationHandler]
    -- ^ Notificationhandlers that are run whenever a new notification is written.
    -> MVar ()
    -- ^ signal that new notifications have been written
    -> MVar ()
    -- ^ stop signal
    -> IO ()
notificationsThread getNotifications deleteNotif handlers newNotifs stopSignal =
    case twoNotifHandlersWithEqualSignature handlers of
      True -> error "two notifhandlers with the same signature were added"
      False -> loop
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
          do notifications <- getNotifications
             forM_ notifications $
                 \(Entity notifId (Notification timestamp payload sig)) ->
                     case Map.lookup sig notifProcessFunktionMap of
                       Just processFunc ->
                           do processFunc timestamp payload
                              deleteNotif notifId
                       Nothing ->
                           error $ "notifications for handler"
                                   ++ (T.unpack sig)
                                   ++ "found but this handler has not been registered!"
             newNotifSignal <- async $ takeMVar newNotifs
             stop <- async $ readMVar stopSignal
             waitEither_ newNotifSignal stop
             loop
      notifProcessFunktionMap =
          Map.fromList $ map (\nh -> (nh_signature nh, nh_processNotif nh)) handlers

      twoNotifHandlersWithEqualSignature [] = False
      twoNotifHandlersWithEqualSignature (nh:nhs) =
          any ((==) (nh_signature nh)) (map nh_signature nhs)
                  || (twoNotifHandlersWithEqualSignature nhs)

