{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings #-}
module NotifSpec
    ( spec
    )
where

import Kis

import Control.Concurrent
import Control.Exception.Base
import Control.Monad
import Control.Monad.Trans.Class
import System.IO.Temp
import Test.Hspec
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

type RequestType = T.Text

spec :: Spec
spec = do
    describe "runKis" $ do
        it "can be run with single service" $ do
          collectedNotifs <- newMVar []
          let client =
                  do bed <- createBed "xy"
                     pat <- createPatient (Patient "Simon")
                     void $ placePatient pat bed
          withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
              runKis [client] [simpleNotifHandler collectedNotifs "notif1"] (T.pack fp)
          notifs <- takeMVar collectedNotifs
          tail notifs `shouldBe`
                   ([ T.pack (show (CreatePatient (Patient "Simon")))
                    , T.pack (show (CreateBed "xy"))
                    ]
                   )
    describe "Notification handlers" $ do
       it "are notified of Notifications" $
           do mvar1 <- newMVar []
              mvar2 <- newMVar []
              let client1 = void $ createPatient $ Patient "Simon"
                  client2 =
                      do void $ createBed "xy"
                         void $ createPatient (Patient "Thomas")
                         void $ getPatients
                  nh1 = simpleNotifHandler mvar1 "notifH1"
                  nh2 = simpleNotifHandler mvar2 "notifH2"
                  allHandlers = [nh1, nh2]
                  allClients = [client1, client2]
                  prop l =
                      T.pack (show (CreatePatient $ Patient "Simon")) `elem` l
                      &&  T.pack (show (CreatePatient $ Patient "Thomas")) `elem` l
                      &&  T.pack (show (CreateBed "xy")) `elem` l
                      && length l == 3
              withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
                  runKis allClients allHandlers (T.pack fp)
              notifList1 <- takeMVar mvar1
              notifList2 <- takeMVar mvar2
              notifList1 `shouldSatisfy` prop
              notifList2 `shouldSatisfy` prop
       it "does not block if nothing happens in service" $
           (withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
               runKis [] [] (T.pack fp))
       -- ^ The reason for this test was a bug where the notifications thread
       -- was blocked and didnt wakeup on the stop signal.
       it "can use the Kis interface to access DB" $
          do resMvar <- newEmptyMVar
             let client =
                     do pat <- createPatient $ Patient "Simon"
                        bed <- createBed "xy"
                        void $ placePatient pat bed
                 saveFunction :: Monad m => (KisRequest a, a) -> WriteNotifFunc m -> KisClient m ()
                 saveFunction (request, _) writeNotif =
                     case request of
                       PlacePatient patId _ ->
                           do patient <- getPatient patId
                              lift $ writeNotif (BSL.toStrict (J.encode patient))
                       _ -> return ()
                 processFunction bs =
                     putMVar resMvar res
                         where Just res = J.decode (BSL.fromStrict bs)
                 nh = NotificationHandler saveFunction processFunction "nh1"
             withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
                 runKis [client] [nh] (T.pack fp)
             result <- takeMVar resMvar
             result `shouldBe` (Patient "Simon")
       it "raises an exception if the save Notif action tries to write into DB" $
          let client = void $ createPatient (Patient "Simon")
              saveFunction :: Monad m => (KisRequest a, a) -> WriteNotifFunc m -> KisClient m ()
              saveFunction _  _ = void $ createPatient (Patient "Thomas")
              processFunction _ = return ()
              nh = NotificationHandler saveFunction processFunction "nh1"
          in (withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
                 runKis [client] [nh] (T.pack fp))
             `shouldThrow` (== ErrorCall "Save-Notification action spawned a write request")

       it "cannot add two notifHandlers with equal signature" $
          let nh1 = NotificationHandler saveRequest (\_ -> return ()) "nh1"
          in (withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
                 runKis [] [nh1, nh1] (T.pack fp))
             `shouldThrow` (== ErrorCall "two notifhandlers with the same signature were added")


simpleNotifHandler :: MVar [RequestType] -> T.Text -> NotificationHandler
simpleNotifHandler notifList sig =
    NotificationHandler
    { nh_saveNotif = saveRequest
    , nh_processNotif = processNotification notifList
    , nh_signature = sig
    }

saveRequest ::
    Monad m
 => (KisRequest a, a)
 -> WriteNotifFunc m
 -> KisClient m ()
saveRequest (request, _) writeNotif =
     lift $ writeNotif (BSL.toStrict (J.encode (T.pack (show request))))

processNotification :: MVar [RequestType] -> BS.ByteString -> IO ()
processNotification notifList payload =
    do oldList <- takeMVar notifList
       let Just reqType = J.decode (BSL.fromStrict payload)
       putMVar notifList (reqType:oldList)
