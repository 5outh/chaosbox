module ChaosBox.Geometry.Quad
  ( QuadOf(..)
  , Quad
  , quad
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon (polygon)
import           ChaosBox.HasV2
import           Data.Foldable             (for_)

data QuadOf a = QuadOf
  { quadA :: a
  , quadB :: a
  , quadC :: a
  , quadD :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance HasV2 a => Affine (QuadOf a) where
  transform = defaultTransform

instance HasV2 a => Draw (QuadOf a) where
  draw QuadOf {..} = for_ (polygon [quadA, quadB, quadC, quadD]) draw

type Quad = QuadOf (V2 Double)

quad :: a -> a -> a -> a -> QuadOf a
quad = QuadOf
