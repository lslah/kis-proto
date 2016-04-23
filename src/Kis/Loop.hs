{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module Kis.Loop
    ( sqlThread
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

sqlThread :: TQueue Request -> IO ()
sqlThread q =
    runStdoutLoggingT $ withSqliteConn ":memory:" $ \backend -> sqlThread' backend q

sqlThread' ::
    SqlBackend
    -> TQueue Request
    -> LoggingT IO ()
sqlThread' backend q =
    runSqlConn (runMigration migrateAll >> loopSql q) backend

loopSql :: TQueue Request -> SqlPersistT (LoggingT IO) ()
loopSql q = forever (process =<< nextItem)
    where
        nextItem = liftIO $ atomically $ readTQueue q
        process (Request req resVar) =
            logReq req >> runAction req >>= resolve resVar
        resolve resVar res = logRes res >> (liftIO . atomically $ putTMVar resVar res)
        logReq req = logInfoN ("Request: " <> (pack . show $ req))
        logRes res = logInfoN ("Response: " <> (pack . show $ res))
