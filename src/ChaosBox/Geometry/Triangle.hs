-- | Triangles
module ChaosBox.Geometry.Triangle
  ( TriangleOf(..)
  , Triangle
  , pattern Triangle
  , triangleA
  , triangleB
  , triangleC
  , triangleCenter
  , translateTriangle
  , scaleTriangle
  , scaleTriangleAround
  , rotateTriangle
  , rotateTriangleAround
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Math (average)
import           ChaosBox.AABB
import           ChaosBox.Draw
import           ChaosBox.Geometry.Angle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Polygon
import           ChaosBox.Geometry.Transform
import           Control.Lens                ((^.))
import           Data.Function               (on)
import           Data.List                   (sortBy)
import           Data.List.NonEmpty          (NonEmpty (..))

data TriangleOf a = TriangleOf
  { triangleOfA :: a
  , triangleOfB :: a
  , triangleOfC :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance HasP2 a => HasAABB (TriangleOf a) where
  aabb = aabb . toPolygon

instance HasP2 a => Draw (TriangleOf a) where
  draw = draw . toPolygon

type Triangle = TriangleOf P2

pattern Triangle :: P2 -> P2 -> P2 -> Triangle
pattern Triangle {triangleA, triangleB, triangleC} = TriangleOf triangleA triangleB triangleC
{-# COMPLETE Triangle #-}

-- TODO: This should exist in a typeclass IsPolygon or triangleToPolygon etc
-- etc
toPolygon :: TriangleOf a -> PolygonOf a
toPolygon TriangleOf {..} = PolygonOf $ triangleOfA :| [triangleOfB, triangleOfC]

instance HasP2 a => Boundary (TriangleOf a) where
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
triangleList TriangleOf {..} = [triangleOfA, triangleOfB, triangleOfC]

sortOnPolarAngle :: (Fractional a, Ord a) => [V2 a] -> [V2 a]
sortOnPolarAngle []       = []
sortOnPolarAngle [x     ] = [x]
sortOnPolarAngle (x : xs) = x : sortBy (compare `on` polarAngle x) xs
  where polarAngle a b = negate $ (b ^. _x - a ^. _x) / (b ^. _y - a ^. _y)

translateTriangle :: HasP2 a => P2 -> TriangleOf a -> TriangleOf a
translateTriangle = translatePoints

scaleTriangle :: HasP2 a => P2 -> TriangleOf a -> TriangleOf a
scaleTriangle = scalePoints

scaleTriangleAround :: HasP2 a => P2 -> P2 -> TriangleOf a -> TriangleOf a
scaleTriangleAround = scaleAroundPoints

rotateTriangle :: HasP2 a => Angle -> TriangleOf a -> TriangleOf a
rotateTriangle = rotatePoints

rotateTriangleAround :: HasP2 a => P2 -> Angle -> TriangleOf a -> TriangleOf a
rotateTriangleAround = rotateAroundPoints

-- | The center of mass of a 'Triangle'
triangleCenter :: HasP2 a => TriangleOf a -> P2
triangleCenter = average . fmap (^._V2)
