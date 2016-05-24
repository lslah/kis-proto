module Main where

import Kis
import Web
import Simulator.Sim
import Simulator.Template

import System.IO.Temp
import qualified Data.Text as T

someServices :: [Service IO]
someServices =
    [ Service webClient (\_ -> return ())
    , Service (runSimulator __template1__) (\_ -> return ())
    ]

main :: IO ()
main =
    withTempFile "/tmp/" "tmpKisDB" $ \fp _ ->
        runKis someServices (T.pack fp)
