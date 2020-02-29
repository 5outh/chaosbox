module ChaosBox.Geometry.Triangle
  ( TriangleOf(..)
  , Triangle
  , triangle
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Geometry.P2
import           Data.Function                  ( on )
import           Control.Lens                   ( (^.) )
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon
import           ChaosBox.AABB
import           ChaosBox.Geometry.Class
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.List                      ( sortBy )

data TriangleOf a = TriangleOf
  { triangleA :: a
  , triangleB :: a
  , triangleC :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Triangle = TriangleOf (P2)

triangle :: a -> a -> a -> TriangleOf a
triangle = TriangleOf

instance HasV2 a => HasAABB (TriangleOf a) where
  aabb = aabb . toPolygon

instance HasV2 a => Affine (TriangleOf a) where
  transform = defaultTransform

instance HasV2 a => Draw (TriangleOf a) where
  draw = draw . toPolygon

-- TODO: This should exist in a typeclass IsPolygon or triangleToPolygon etc
-- etc
toPolygon :: TriangleOf a -> PolygonOf a
toPolygon TriangleOf {..} = PolygonOf $ triangleA :| [triangleB, triangleC]

instance HasV2 a => Boundary (TriangleOf a) where
  containsPoint t p = b1 == b2 && b2 == b3
   where
    [t1, t2, t3] = sortOnPolarAngle $ map (^. _V2) $ triangleList t
    sign p1 p2 p3 =
      (p1 ^. _x - p3 ^. _x)
        * (p2 ^. _y - p3 ^. _y)
        - (p2 ^. _x - p3 ^. _x)
        * (p1 ^. _y - p3 ^. _y)
    b1 = sign p t1 t2 < 0
    b2 = sign p t2 t3 < 0
    b3 = sign p t3 t1 < 0

triangleList :: TriangleOf a -> [a]
triangleList TriangleOf {..} = [triangleA, triangleB, triangleC]

sortOnPolarAngle :: (Fractional a, Ord a) => [V2 a] -> [V2 a]
sortOnPolarAngle []       = []
sortOnPolarAngle [x     ] = [x]
sortOnPolarAngle (x : xs) = x : sortBy (compare `on` polarAngle x) xs
  where polarAngle a b = negate $ (b ^. _x - a ^. _x) / (b ^. _y - a ^. _y)
