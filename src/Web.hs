{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Web
    ( webMain
    , inMemoryApplication
    )
where

import Control.Monad.RWS hiding (get)
import Network.Wai.Middleware.Static
import Network.Wai
import Web.Spock.Safe

import Kis
import Kis.SqlBackend
import Kis.Time

__ASSET_DIR__ :: FilePath
__ASSET_DIR__ = "frontend"

webMain :: IO ()
webMain = runSpock 8080 inMemoryWeb

web :: MonadIO m => (forall a. KisClient m a -> IO a) -> IO Middleware
web runKisClient =
    spockT runKisClient webInterface

inMemoryWeb :: IO Middleware
inMemoryWeb = do
    backend <- sqliteBackend InMemory
    web (runClient backend (KisConfig realTimeClock))

inMemoryApplication :: IO Application
inMemoryApplication = spockAsApp inMemoryWeb

webInterface :: MonadIO m => SpockT (KisClient m) ()
webInterface = do
    middleware $ staticPolicy $ addBase __ASSET_DIR__
    post "/patient" $ do
        patient <- jsonBody'
        patId <- req' (CreatePatient patient)
        json patId
    get "/patients" $ do
        patients <- req' GetPatients
        json patients
    where req' = lift . req
