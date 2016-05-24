module NotifSpec
    ( spec
    )
where

import Kis

--import Control.Concurrent
--import Control.Monad
--import Control.Monad.IO.Class
--import System.IO.Temp
import Test.Hspec
--import qualified Data.Text as T

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
                notifications <- (k_getNotifications kis)
                notifications `shouldBe` (map Notification [NewBed, NewPatient, PatientMoved])
    -- describe "runKis" $
    --    it "can be run without services" $
    --        withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
    --            runKis [] (T.pack fp)

    -- describe "Notification handlers" $
    --    it "are notified of Notifications" $ do
    --          mvar1 <- newMVar []
    --          done <- newEmptyMVar
    --          let client1 = void $ req (CreatePatient $ Patient "Simon")
    --              services = [ Service client1 (notifHandler done mvar1) ]
    --          withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
    --             runKis services (T.pack fp)
    --          _ <- takeMVar done
    --          notifList1 <- takeMVar mvar1
    --          notifList1 `shouldBe` [Notification NewPatient]

kisConfig :: KisConfig
kisConfig = KisConfig realTimeClock

-- notifHandler :: MonadIO m => MVar () -> MVar [Notification] -> Notification -> KisClient m ()
-- notifHandler done notifList newNotif =
--     do oldList <- liftIO $ takeMVar notifList
--        liftIO $ putMVar notifList (newNotif:oldList)
--        liftIO $ putMVar done ()
