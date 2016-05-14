module Kis.Time
   ( TimeOffset(..)
   , KisTime(..)
   )
where

import Data.Time.Clock

newtype TimeOffset = TimeOffset DiffTime

-- | KisTime indicates wheter KIS is running in real time,
-- or in virtual time (for simulation). If running in Virtual
-- time the time multiplier and starting point of the simulation
-- are in the Type aswell
data KisTime = RealTime | VirtualTime UTCTime Int

-- data VirtualTime =
--     VirtualTime
--     { vt_startingPoint :: UTCTime
--     , vt_multiplier :: Int
--     }
