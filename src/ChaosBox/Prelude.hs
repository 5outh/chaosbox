module ChaosBox.Prelude
  ( lerpMany
  , module X
  )
where

import           Linear as X

-- | N lerps between two vectors, exclusive on upper bound
lerpMany
  :: (Num a, Additive f, Fractional a, Enum a) => Int -> f a -> f a -> [f a]
lerpMany n p q = map (\c -> lerp c p q) constants
 where
  step      = 1 / fromIntegral n
  constants = [0, step .. fromIntegral n - step]
