{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module Kis.Loop
    ( kisThread
    , request
    , Request
    , KisAction(..)
    )
where

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.STM
import Control.Monad.Trans.Reader
import Database.Persist.Sqlite
import Data.Monoid
import Data.Text

import Kis.Model
import Kis.Backend

-- | Contains a KisAction and a TMVar which will be filled with the result of
-- the action by the sqlThread. The return type should be instance of Show in
-- order to log the result of our request.
data Request where
    Request :: Show a => KisAction a -> TMVar a -> Request

data KisAction a where
    CreatePatient :: String -> KisAction PatientId
    CreateBed :: String -> KisAction BedId
    PlacePatient :: PatientId -> BedId -> KisAction (Maybe PatientBedId)
    GetPatients :: KisAction [Entity Patient]

runAction :: MonadIO m => KisAction a -> ReaderT SqlBackend m a
runAction (CreatePatient name) = createPatient name
runAction (CreateBed name) = createBed name
runAction (PlacePatient patId bedId) = placePatient patId bedId
runAction GetPatients = getPatients

instance Show (KisAction a) where
    show (CreatePatient name) = "Create patient " <> name
    show (CreateBed name) = "Create bed " <> name
    show (PlacePatient pid bid) = "Place patient " <> show pid <> " in bed " <> show bid
    show GetPatients = "Get patients"

instance Show Request where
    show (Request action _) = show action

-- | Place a KisAction in the request queue and wait for the result.
request :: (MonadIO m, Show a) => TQueue Request -> KisAction a -> m a
request q action =
    liftIO $ do
       res <- newEmptyTMVarIO
       atomically $ writeTQueue q (Request action res)
       atomically $ takeTMVar res

kisThread :: TQueue Request -> IO ()
kisThread q =
    runStdoutLoggingT $ withSqliteConn ":memory:" $ \backend -> do
        runSqlConn (runMigration migrateAll) backend
        loop q backend

loop :: TQueue Request -> SqlBackend -> LoggingT IO ()
loop q backend =
    forever (process =<< nextRequest)
    where
      process (Request req resVar) = do
          logShow "Request" req
          res <- runSqlRequest req backend
          logShow "Result" res
          resolve resVar res
      nextRequest = liftIO $ atomically $ readTQueue q
      resolve resVar res = liftIO . atomically $ putTMVar resVar res

runSqlRequest :: KisAction a -> SqlBackend -> LoggingT IO a
runSqlRequest req backend = runSqlConn (runAction req) backend

logShow :: (MonadLogger m, Show a) => Text -> a -> m ()
logShow tag x = logInfoN (tag <> ": " <> (pack . show $ x))

