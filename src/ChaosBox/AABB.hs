-- | Minimal axis-alignedbounding boxes by an axis-aligned bounding box
module ChaosBox.AABB
  ( HasAABB(..)
  , AABB(..)
  , boundary
  )
where

import           Linear.V2
import           Data.List.NonEmpty
import           ChaosBox.Geometry.Class        ( HasV2(..) )
import           Control.Lens                   ( (^.) )
import           ChaosBox.Geometry.P2

-- | An Axis-Aligned Bounding Box
data AABB = AABB
  { aabbTopLeft :: P2
  , aabbW       :: Double
  , aabbH       :: Double
  }
  deriving stock (Show, Eq, Ord)

class HasAABB shape where
  aabb :: shape -> AABB

-- | Get the bounds of a list of positioned objects.
boundary :: HasV2 a => NonEmpty a -> AABB
boundary xs = AABB tl w h
 where
  l        = toList xs
  tl       = minimum $ fmap (^. _V2) l
  br       = maximum $ fmap (^. _V2) l
  (V2 w h) = br - tl
