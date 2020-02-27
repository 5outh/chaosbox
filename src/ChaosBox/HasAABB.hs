-- | Class of items that can be minimally bounded by an axis-aligned bounding box
module ChaosBox.HasAABB
  ( HasAABB(..)
  )
where

import           ChaosBox.Geometry.Rect (Rect)

class HasAABB shape where
  aabb :: shape -> Rect
