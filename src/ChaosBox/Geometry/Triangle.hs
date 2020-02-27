module ChaosBox.Geometry.Triangle
  ( TriangleOf(..)
  , Triangle
  , triangle
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon
import qualified ChaosBox.Geometry.Rect    as Rect
import           ChaosBox.HasAABB
import           ChaosBox.HasV2
import           Data.List.NonEmpty        (NonEmpty (..))

data TriangleOf a = TriangleOf
  { triangleA :: a
  , triangleB :: a
  , triangleC :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Triangle = TriangleOf (V2 Double)

triangle :: a -> a -> a -> TriangleOf a
triangle = TriangleOf

instance HasV2 a => HasAABB (TriangleOf a) where
  aabb = Rect.bounds . toPolygon

instance HasV2 a => Affine (TriangleOf a) where
  transform = defaultTransform

instance HasV2 a => Draw (TriangleOf a) where
  draw = draw . toPolygon

-- TODO: This should exist in a typeclass IsPolygon or triangleToPolygon etc
-- etc
toPolygon :: TriangleOf a -> PolygonOf a
toPolygon TriangleOf {..} = PolygonOf $ triangleA :| [triangleB, triangleC]
