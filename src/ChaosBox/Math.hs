module ChaosBox.Math
  ( lerp
  , lerpMany
  , average
  , module X
  )
where

import           ChaosBox.Math.Matrix as X
import           ChaosBox.Math.Vector as X

import           Data.Foldable        (toList)
import           Data.List            (genericLength)

-- | Linearly interpolate between two numbers
--
-- > lerp 0.5 0 10 == 5
--
lerp :: Num a => a -> a -> a -> a
lerp perc a b = (perc - 1) * a + perc * b

-- | N lerps between two points, exclusive on upper bound
lerpMany :: (Num a, Fractional a, Enum a) => Int -> a -> a -> [a]
lerpMany n p q = map (\c -> lerp c p q) constants
 where
  step      = 1 / fromIntegral n
  constants = [0, step .. fromIntegral n - step]

average :: (Num a, Fractional a, Foldable f) => f a -> a
average xs = sum xs0 / genericLength xs0 where xs0 = toList xs
