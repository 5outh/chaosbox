-- | Minimally axis-aligned bounding boxes
module ChaosBox.AABB
  ( HasAABB(..)
  , AABB(..)
  , boundary
  , aabbContains
  )
where

import           ChaosBox.Geometry.Class (HasP2 (..))
import           ChaosBox.Geometry.P2
import           Control.Lens            ((^.))
import           Data.List.NonEmpty
import           Linear.V2

-- | An Axis-Aligned Bounding Box
data AABB = AABB
  { aabbTopLeft :: P2
  , aabbW       :: Double
  , aabbH       :: Double
  }
  deriving stock (Show, Eq, Ord)

-- | Class of types that can be minimally bounded by an 'AABB'
class HasAABB shape where
  aabb :: shape -> AABB

-- | Get the bounds of a list of positioned objects.
boundary :: HasP2 a => NonEmpty a -> AABB
boundary xs = AABB tl w h
 where
  l        = toList xs
  tl       = minimum $ fmap (^. _V2) l
  br       = maximum $ fmap (^. _V2) l
  (V2 w h) = br - tl

-- | Check if an 'AABB' contains some 2d point ('P2')
aabbContains :: AABB -> P2 -> Bool
aabbContains AABB {..} (P2 x y) =
  x >= x0 && x < x0 + aabbW && y >= y0 && y < y0 + aabbH
  where V2 x0 y0 = aabbTopLeft
