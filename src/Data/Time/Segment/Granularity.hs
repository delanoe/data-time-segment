module Data.Time.Segment.Granularity where

import qualified Safe as S (headMay)
import Data.Time (UTCTime, getCurrentTime, NominalDiffTime)

-- Grain is time size/density
data Granularity =
    M   | W   | D   |
    H12 | H8  | H6  | H4  | H3 | H2 | H1 |
    M30 | M15 | M10 | M5  | M1 |
    -- M30 | M15 | M10 | M5  | M4 | M3 | M2 | M1 |
    S30 | S15 | S10 | S5 
   deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- grains ?
allGranularity :: [Granularity]
allGranularity = [minBound..maxBound]

grain2seconds :: Granularity -> Int
grain2seconds grain = case grain of
                        S5  -> 5
                        S10 -> 10
                        S15 -> 15
                        S30 -> 30
                        M1  -> 1 * 60
                        -- M2  -> 2 * 60
                        -- M3  -> 3 * 60
                        -- M4  -> 4 * 60
                        M5  -> 5 * 60
                        M10 -> 10* 60
                        M15 -> 15* 60
                        M30 -> 30* 60
                        H1  -> 1 * 60 * 60
                        H2  -> 2 * 60 * 60
                        H3  -> 3 * 60 * 60
                        H4  -> 4 * 60 * 60
                        H6  -> 6 * 60 * 60
                        H8  -> 8 * 60 * 60
                        H12 -> 12* 60 * 60
                        D   -> 24* 60 * 60
                        W   -> 7 * 24 * 60 * 60
                        M   -> 31* 24 * 60 * 60

grain2seconds' :: Granularity -> NominalDiffTime
grain2seconds' grain = case grain of
                        S5  -> 5
                        S10 -> 10
                        S15 -> 15
                        S30 -> 30
                        M1  -> 1 * 60
                        -- M2  -> 2 * 60
                        -- M3  -> 3 * 60
                        -- M4  -> 4 * 60
                        M5  -> 5 * 60
                        M10 -> 10* 60
                        M15 -> 15* 60
                        M30 -> 30* 60
                        H1  -> 1 * 60 * 60
                        H2  -> 2 * 60 * 60
                        H3  -> 3 * 60 * 60
                        H4  -> 4 * 60 * 60
                        H6  -> 6 * 60 * 60
                        H8  -> 8 * 60 * 60
                        H12 -> 12* 60 * 60
                        D   -> 24* 60 * 60
                        W   -> 7 * 24 * 60 * 60
                        M   -> 31* 24 * 60 * 60



seconds2grain :: Int -> Granularity
seconds2grain seconds = do
    let grain = S.headMay $ filter (\x -> grain2seconds x == seconds) allGranularity
    case grain of
      Just grain'  -> grain'
      Nothing -> M


grain2grain :: Granularity -> Int -> Granularity
grain2grain g s = do
    if ( s > 0 ) 
       then grain2grainSup g s
    else 
        grain2grainInf g (abs s)

grain2grainSup :: Granularity -> Int -> Granularity
grain2grainSup g s = do
    let seconds = (grain2seconds g) * s
    seconds2grain seconds


grain2grainInf :: Granularity -> Int -> Granularity
grain2grainInf g s = do
    let seconds = (grain2seconds g) `div` s
    seconds2grain seconds


