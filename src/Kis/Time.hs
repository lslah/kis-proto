module Kis.Time
   ( TimeOffset(..)
   , Clock(..)
   , realTimeClock
   , virtualTimeClock
   , constClock
   )
where

import Control.Monad
import Data.Time.Clock
import System.Time.Extra

newtype TimeOffset = TimeOffset DiffTime

data Clock m =
    Clock
    { c_getTime :: m UTCTime
    , c_waitFor :: TimeOffset -> m ()
    , c_waitUntil :: UTCTime -> m ()
    }

getVirtualTime ::
       UTCTime -- Reference timestamp for t = 0
    -> Double  -- Time multiplier
    -> IO UTCTime
getVirtualTime ref mult =
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
     when (timeToWait >= 0) $
          waitForVirtualTime mult (TimeOffset $ convert timeToWait)

waitUntilRealTime :: UTCTime -> IO ()
waitUntilRealTime time =
  do now <- getCurrentTime
     let timeToWait = diffUTCTime time now
     when (timeToWait >= 0) (sleep $ convert timeToWait)

realTimeClock :: Clock IO
realTimeClock =
    Clock
    { c_getTime = getCurrentTime
    , c_waitFor = \(TimeOffset diffTime) -> sleep $ convert diffTime
    , c_waitUntil = waitUntilRealTime
    }

virtualTimeClock ::
       UTCTime
    -> Double
    -> Clock IO
virtualTimeClock ref mult =
    Clock
    { c_getTime = getVirtualTime ref mult
    , c_waitFor = waitForVirtualTime mult
    , c_waitUntil = waitUntilVirtualTime ref mult
    }

constClock :: UTCTime -> Clock IO
constClock time =
    Clock
    { c_getTime = return time
    , c_waitFor = \_ -> return ()
    , c_waitUntil = \_ -> return ()
    }

convert :: (Real a, Fractional b) => a -> b
convert = fromRational . toRational
