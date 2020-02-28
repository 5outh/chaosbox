module ChaosBox.Math.Vector
  ( orient
  , lerpV
  , lerpManyV
  )
where

import qualified Linear        as L
import           Linear.Matrix (det33)
import           Linear.V2     (V2 (..))
import           Linear.V3     (V3 (..))

-- brittany-disable-next-binding

-- | Orient three points
--
-- 'GT' means they are ordered counterclockwise.
-- 'LT' means they are ordered clockwise.
-- 'EQ' means they lie on the same line.
--
orient :: (Num a, Ord a) => V2 a -> V2 a -> V2 a -> Ordering
orient (V2 px py) (V2 qx qy) (V2 rx ry) = compare orientation 0
  where
    orientation = det33 $ V3
      (V3 1 px py)
      (V3 1 qx qy)
      (V3 1 rx ry)

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
