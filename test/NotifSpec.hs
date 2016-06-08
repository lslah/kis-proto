module NotifSpec
    ( spec
    )
where

import Kis

import Control.Concurrent
import Control.Exception.Base
import Control.Monad
import Control.Monad.IO.Class
import Database.Persist
import Data.List.Utils (countElem)
import System.IO.Temp
import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Notifications" $
        it "are correctly written to DB" $ do
            withSqliteKis InMemory kisConfig $ \kis -> do
                runClient kis $ do
                    bed <- req (CreateBed "xy")
                    pat <- req (CreatePatient $ Patient "Simon")
                    _ <- req (PlacePatient pat bed)
                    return ()
                let ns = k_notificationSystem kis
                notifications <- fmap (map (\(Entity _ notif) -> notif)) (ns_getNotifications ns)
                notifications `shouldBe` (map Notification [NewBed, NewPatient, PatientMoved])
    describe "runKis" $ do
        it "can be run with single service" $ do
          collectedNotifs <- newMVar []
          let client =
                  do bed <- req (CreateBed "xy")
                     pat <- req (CreatePatient $ Patient "Simon")
                     void $ req (PlacePatient pat bed)
              service = Service client (notifHandler collectedNotifs)
          withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
              runKis [service] (T.pack fp)
          notifs <- takeMVar collectedNotifs
          notifs `shouldBe` (map Notification [PatientMoved, NewPatient, NewBed])
    describe "Notification handlers" $ do
       it "are notified of Notifications" $ do
             mvar1 <- newMVar []
             mvar2 <- newMVar []
             let client1 =
                     void $ req (CreatePatient $ Patient "Simon")
                 client2 =
                     do void $ req (CreateBed "xy")
                        void $ req (CreatePatient $ Patient "Thomas")
                        void $ req GetPatients
                 services = [ Service client1 (notifHandler mvar1)
                            , Service client2 (notifHandler mvar2)
                            ]
                 prop l = countElem (Notification NewPatient) l == 2
                          && countElem (Notification NewBed) l == 1
                          && length l == 3
             withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
                runKis services (T.pack fp)
             notifList1 <- takeMVar mvar1
             notifList2 <- takeMVar mvar2
             notifList1 `shouldSatisfy` prop
             notifList2 `shouldSatisfy` prop
       it "does not block if nothing happens in service" $
           (withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
               runKis [Service (return ()) (\_ -> return ())] (T.pack fp))
       -- ^ The reason for this test is that the newNotif MVar was blocked indefinitely
       -- and the notifications thread didnt wakeup on the stop signal.
       it "exceptions in notification handler is catched" $
           (withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
               runKis [Service (void $ req (CreateBed "")) notifHandlerWithException] (T.pack fp))
           `shouldThrow` (== Overflow)


kisConfig :: KisConfig
kisConfig = KisConfig realTimeClock

notifHandler :: MonadIO m => MVar [Notification] -> Notification -> KisClient m ()
notifHandler notifList newNotif =
    do oldList <- liftIO $ takeMVar notifList
       liftIO $ putMVar notifList (newNotif:oldList)

notifHandlerWithException :: MonadIO m => Notification -> KisClient m ()
notifHandlerWithException _ = throw Overflow
