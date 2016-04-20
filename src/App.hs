module App
    ( appMain
    )
where

import Kis

import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Monad

appMain :: IO ()
appMain = do
    q <- newTQueueIO
    withAsync
        (sqlThread q)
        (\_ -> main' q)

main' :: TQueue Request -> IO ()
main' q = do
    thomasId <- request q (createPatient "Thomas")
    bedId <- request q (createBed "Thomas' Bed")
    void $ request q (createPatient "Pia")
    void $ request q (placePatient thomasId bedId)
    void $ request q getPatients
