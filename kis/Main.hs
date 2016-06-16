module Main where

import Kis
import Web
import Simulator.Sim
import Simulator.Template

import System.IO.Temp
import qualified Data.Text as T

someClients :: [KisClient IO ()]
someClients =
    [ webClient
    , (runSimulator __template1__)
    ]

main :: IO ()
main =
    withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
        runKis someClients [] (T.pack fp)
