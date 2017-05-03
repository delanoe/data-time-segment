module Data.Time.Segment.Main where

import Data.Time.Segment.Granularity

import Prelude
import Debug.Trace (trace)
import Data.Time
import Data.Time.Clock (utctDay)
import Data.List (groupBy, scanl', foldl')
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)

import Data.Fixed (Pico)
import Control.Concurrent (threadDelay)


-----------------------------------------------------------------
date :: Integer -> Int -> Int -> Int -> Int -> Pico -> UTCTime
date y m d h mn s = UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay h mn s)

jour :: Integer -> Int -> Int -> UTCTime
jour y m d = date y m d 0 0 0

begin:: UTCTime
begin = date 2017 03 06 0 0 0

end :: UTCTime
end   = date 2017 03 04 0 0 0

-----------------------------------------------------------------
-----------------------------------------------------------------
data Times = Before | After
  deriving (Eq, Show, Read)

times :: Times -> Int -> Granularity -> UTCTime -> [UTCTime]
times t n g d = chronos scanl' t n g d

timeIt :: Times -> Int -> Granularity -> UTCTime -> UTCTime
timeIt t n g d = chronos foldl' t n g d

chronos :: (Num t3, Enum t3) => ((UTCTime -> t -> UTCTime)
      -> t2 -> [t3] -> t1) -> Times -> t3 -> Granularity -> t2 -> t1
chronos f t n g d = f (\n' _ -> addUTCTime (sign sec) n') d [1..n]
    where
        sec  = grain2seconds' g
        sign = case t of
                  Before  -> negate
                  _       -> id
-----------------------------------------------------------------
-- | After
timesAfter :: Int -> Granularity -> UTCTime -> [UTCTime]
timesAfter n g d = times After n g d

-- | Before
timesBefore :: Int -> Granularity -> UTCTime -> [UTCTime]
timesBefore n g d = times Before n g d

-- | After now
timesAfterNow :: Int -> Granularity -> IO [UTCTime]
timesAfterNow n g = times After n g <$> getCurrentTime

-- | Before now
timesBeforeNow :: Int -> Granularity -> IO [UTCTime]
timesBeforeNow n g = times Before n g <$> getCurrentTime

-----------------------------------------------------------------
minutesAfter :: Int -> UTCTime -> [UTCTime]
minutesAfter n d = timesAfter n M1 d

minutesBefore :: Int -> UTCTime -> [UTCTime]
minutesBefore n d = timesBefore n M1 d

-----------------------------------------------------------------
minutesAfterNow :: Int -> IO [UTCTime]
minutesAfterNow n = timesAfter n M1 <$> getCurrentTime

minutesBeforeNow :: Int -> IO [UTCTime]
minutesBeforeNow n = timesBefore n M1 <$> getCurrentTime


-----------------------------------------------------------------
timesBetween :: UTCTime -> UTCTime -> Granularity -> [UTCTime]
timesBetween begin end grain | begin == end = [begin]
                             | begin > end  = [] 
                             | begin < end  = scanl (\n' _ -> addUTCTime sec n') begin [1..end']
                                   where
                                       sec  = grain2seconds' grain
                                       end' = round $ (diffUTCTime end begin) / sec


-----------------------------------------------------------------
sleepToNextMinute :: IO ()
sleepToNextMinute = do t <- getCurrentTime
                       let secs = round (realToFrac $ utctDayTime t) `rem` 60
                       trace (show secs) $ threadDelay $ 10^6 * (60 - secs)

-----------------------------------------------------------------
