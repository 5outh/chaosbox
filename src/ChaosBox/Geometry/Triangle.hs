module ChaosBox.Geometry.Triangle
  ( TriangleOf(..)
  , Triangle
  , triangle
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.HasV2
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon
import           Data.Foldable                  ( for_ )

data TriangleOf a = TriangleOf
  { triangleA      :: a
  , triangleB      :: a
  , triangleC      :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Triangle = TriangleOf (V2 Double)

triangle :: a -> a -> a -> TriangleOf a
triangle = TriangleOf

instance HasV2 a => Affine (TriangleOf a) where
  transform = defaultTransform

instance HasV2 a => Draw (TriangleOf a) where
  draw TriangleOf {..} = for_ (polygon [triangleA, triangleB, triangleC])
    draw
