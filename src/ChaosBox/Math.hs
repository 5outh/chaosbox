module ChaosBox.Math
  ( lerp
  , lerpMany
  , average
  , clamp
  , resample
  , module X
  )
where

import           ChaosBox.Math.Matrix as X
import           ChaosBox.Math.Vector as X

import           Data.Foldable        (toList)
import           Data.List            (genericLength)

-- | Linearly interpolate between two numbers
--
-- prop> lerp 0.5 0 10 = 5
--
lerp :: Num a => a -> a -> a -> a
lerp perc a b = (1 - perc) * a + perc * b

-- | @n@ lerps between two points, exclusive on upper bound
--
-- prop> lerpMany 10 0 8 = [0.0,0.8,1.6,2.4,3.2,4.0,4.8,5.6,6.4,7.2]
--
lerpMany :: (Num a, Fractional a, Enum a) => Int -> a -> a -> [a]
lerpMany n p q = map (\c -> lerp c p q) constants
 where
  step      = 1 / fromIntegral n
  constants = [0, step .. (1 - step)]

-- | Average a 'Foldable' collection
average :: (Num a, Fractional a, Foldable f) => f a -> a
average xs = sum xs0 / genericLength xs0 where xs0 = toList xs

-- | Resample a value in one interval to a value in another
--
-- prop> resample (0,1) (10,20) 0.5 = 15
--
resample :: (Double, Double) -> (Double, Double) -> Double -> Double
resample (start, end) (newStart, newEnd) x =
  newStart + ((newEnd - newStart) * perc)
 where
  -- The size of the initial interval
  size = end - start
  -- amount "through" the intitial interval we are
  perc = (x - start) / size

-- | Clamp a value in some range
clamp :: Ord a => (a, a) -> a -> a
clamp (rangeStart, rangeEnd) x = max rangeStart (min x rangeEnd)

