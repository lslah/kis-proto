module Kis.Time
   ( TimeOffset(..)
   , Clock(..)
   , realTimeClock
   , virtualTimeClock
   )
where

import Data.Time.Clock
import System.Time.Extra

newtype TimeOffset = TimeOffset DiffTime

data Clock =
    Clock
    { c_getTime :: IO UTCTime
    , c_waitFor :: TimeOffset -> IO ()
    , c_waitUntil :: UTCTime -> IO ()
    }

getVirtualTime ::
       UTCTime -- Reference timestamp for t = 0
    -> Double  -- Time multiplier
    -> IO UTCTime
getVirtualTime ref mult =
  case mult of
  -- | running in real time
   1 -> getCurrentTime
  -- | running in virtual time
   _ ->
     do now <- getCurrentTime
        let timeElapsed = diffUTCTime now ref
            virtualDiffTime = timeElapsed * (convert mult)
        return $ addUTCTime virtualDiffTime ref

waitForVirtualTime ::
    Double -- Time multiplier
    -> TimeOffset
    -> IO ()
waitForVirtualTime mult (TimeOffset diffTime) =
    sleep $ diffTimeAsDouble / mult
    where
      diffTimeAsDouble = convert diffTime :: Double

waitUntilVirtualTime ::
       UTCTime -- Reference timestamp for t = 0
    -> Double -- Time multiplier
    -> UTCTime -- time to wakeup
    -> IO ()
waitUntilVirtualTime ref mult time =
  do virtNow <- getVirtualTime ref mult
     let timeToWait = diffUTCTime time virtNow
     case timeToWait < 0 of
      True -> return ()
      False -> waitForVirtualTime mult (TimeOffset $ convert timeToWait)

waitUntilRealTime :: UTCTime -> IO ()
waitUntilRealTime time =
  do now <- getCurrentTime
     let timeToWait = diffUTCTime time now
     case timeToWait < 0 of
      True -> return ()
      False -> sleep $ convert timeToWait

realTimeClock :: Clock
realTimeClock =
    Clock
    { c_getTime = getCurrentTime
    , c_waitFor = \(TimeOffset diffTime) -> sleep $ convert diffTime
    , c_waitUntil = waitUntilRealTime
    }

virtualTimeClock ::
       UTCTime
    -> Double
    -> Clock
virtualTimeClock ref mult =
    Clock
    { c_getTime = getVirtualTime ref mult
    , c_waitFor = waitForVirtualTime mult
    , c_waitUntil = waitUntilVirtualTime ref mult
    }

convert :: (Real a, Fractional b) => a -> b
convert = fromRational . toRational
