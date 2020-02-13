module ChaosBox.Math
  ( lerp
  , lerpMany
  , lerpV
  , lerpManyV
  )
where

import qualified Linear as L

lerp :: Num a => a -> a -> a -> a
lerp perc a b = (perc - 1) * a + perc * b

-- | N lerps between two points, exclusive on upper bound
lerpMany :: (Num a, Fractional a, Enum a) => Int -> a -> a -> [a]
lerpMany n p q = map (\c -> lerp c p q) constants
 where
  step      = 1 / fromIntegral n
  constants = [0, step .. fromIntegral n - step]

-- | Lerp between two vectors
lerpV :: (Num a, L.Additive f) => a -> f a -> f a -> f a
lerpV = L.lerp

-- | N lerps between two vectors, exclusive on upper bound
lerpManyV
  :: (Num a, L.Additive f, Fractional a, Enum a) => Int -> f a -> f a -> [f a]
lerpManyV n p q = map (\c -> L.lerp c p q) constants
 where
  step      = 1 / fromIntegral n
  constants = [0, step .. fromIntegral n - step]

