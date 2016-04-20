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
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.STM
import Database.Persist.Sqlite
import Data.Monoid
import Data.Text

import Kis.Model

data Request where
    Request :: Show a => KisAction a -> TMVar a -> Request

-- | A database action together with a description of the action. The
-- description is used to log requests in the sql loop when they appear. Adding
-- more meta data is possible.
data KisAction a =
    KisAction
    { description :: String
    , runAction :: SqlPersistT (LoggingT IO) a
    }

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
loopSql q = nextItem >>= process >> loopSql q
    where
        nextItem = liftIO $ atomically $ readTQueue q
        process (Request req resVar) =
            logReq req >> runAction req >>= resolve resVar
        resolve resVar res = logRes res >> (liftIO . atomically $ putTMVar resVar res)
        logReq req = logInfoN ("Request: " <> (pack . description $ req))
        logRes res = logInfoN ("Response: " <> (pack . show $ res))
