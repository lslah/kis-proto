module NotifSpec
    ( spec
    )
where

import Kis

import Control.Concurrent
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
                notifications <- fmap (map (\(Entity _ notif) -> notif)) (k_getNotifications kis)
                notifications `shouldBe` (map Notification [NewBed, NewPatient, PatientMoved])
    describe "runKis" $ do
        it "can be run with single service" $ do
          collectedNotifs <- newMVar []
          done <- newEmptyMVar
          let client =
                  do bed <- req (CreateBed "xy")
                     pat <- req (CreatePatient $ Patient "Simon")
                     void $ req (PlacePatient pat bed)
                     liftIO $ putMVar done ()
              service = Service client (notifHandler collectedNotifs)
          withTempFile "/tmp/" "tmpKisDB1" $ \fp _ ->
              runKis [service] (T.pack fp)
          _ <- takeMVar done
          notifs <- takeMVar collectedNotifs
          notifs `shouldBe` (map Notification [NewBed, NewPatient, PatientMoved])
    describe "Notification handlers" $
       it "are notified of Notifications" $ do
             mvar1 <- newMVar []
             mvar2 <- newMVar []
             done1 <- newEmptyMVar
             done2 <- newEmptyMVar
             let client1 =
                     do void $ req (CreatePatient $ Patient "Simon")
                        liftIO $ putMVar done1 ()
                 client2 =
                     do void $ req (CreateBed "xy")
                        void $ req (CreatePatient $ Patient "Thomas")
                        void $ req GetPatients
                        liftIO $ putMVar done2 ()
                 services = [ Service client1 (notifHandler mvar1)
                            , Service client2 (notifHandler mvar2)
                            ]
             withTempFile "/tmp/" "tmpKisDB2" $ \fp _ ->
                runKis services (T.pack fp)
             _ <- takeMVar done1
             _ <- takeMVar done2
             notifList1 <- takeMVar mvar1
             notifList2 <- takeMVar mvar2
             notifList1 `shouldSatisfy` prop
             notifList2 `shouldSatisfy` prop
           where
             prop l = countElem (Notification NewPatient) l == 2
                      && countElem (Notification NewBed) l == 1
                      && length l == 3

kisConfig :: KisConfig
kisConfig = KisConfig realTimeClock

notifHandler :: MonadIO m => MVar [Notification] -> Notification -> KisClient m ()
notifHandler notifList newNotif =
    do oldList <- liftIO $ takeMVar notifList
       liftIO $ putMVar notifList (newNotif:oldList)
