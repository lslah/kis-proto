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

__ASSET_DIR__ :: FilePath
__ASSET_DIR__ = "frontend"

webMain :: IO ()
webMain = runSpock 8080 inMemoryWeb

web :: MonadIO m => (forall a. KisClient m a -> IO a) -> IO Middleware
web runKisClient =
    spockT runKisClient webInterface

inMemoryWeb :: IO Middleware
inMemoryWeb = do
    backend <- inMemoryBackend
    web (runClient backend)

inMemoryApplication :: IO Application
inMemoryApplication = spockAsApp inMemoryWeb

webInterface :: MonadIO m => SpockT (KisClient m) ()
webInterface = do
    middleware $ staticPolicy $ addBase __ASSET_DIR__
    post ("/patient" <//> var) $ \name -> do
        patId <- lift $ req (CreatePatient name)
        json patId
    get "/patients" $ do
        patients <- lift $ req GetPatients
        json patients
