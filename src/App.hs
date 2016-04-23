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
    thomasId <- request q (CreatePatient "Thomas")
    bedId <- request q (CreateBed "Thomas' Bed")
    void $ request q (CreatePatient "Pia")
    void $ request q (PlacePatient thomasId bedId)
    void $ request q GetPatients
