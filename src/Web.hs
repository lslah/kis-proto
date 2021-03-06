{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Web
    ( webMain
    , webClient
    , inMemoryApplication
    )
where

import Control.Monad.RWS hiding (get)
import Network.Wai.Middleware.Static
import Network.Wai
import Web.Spock.Safe

import Kis
import Kis.SqliteBackend

__ASSET_DIR__ :: FilePath
__ASSET_DIR__ = "frontend"

webClient :: KisClient IO ()
webClient =
    do kis <- ask
       liftIO $ runSpock 8080 $ web (\kc -> runClient kis kc)

webMain :: IO ()
webMain = runSpock 8080 inMemoryWeb

web :: MonadIO m => (forall a. KisClient m a -> IO a) -> IO Middleware
web runKisClient =
    spockT runKisClient webInterface

inMemoryWeb :: IO Middleware
inMemoryWeb = do
    backend <- sqliteBackend InMemory
    (kis, _) <- buildKisWithBackend backend (KisConfig realTimeClock) NoNotifs
    web (runClient kis)

inMemoryApplication :: IO Application
inMemoryApplication = spockAsApp inMemoryWeb

webInterface :: (KisRead m, KisWrite m) => MonadIO m => SpockT m ()
webInterface = do
    middleware $ staticPolicy $ addBase __ASSET_DIR__
    post "/patient" $ do
        patient <- jsonBody'
        patId <- lift $ createPatient patient
        json patId
    get "/patients" $ do
        patients <- lift getPatients
        json patients
