{-# OPTIONS_GHC -fno-warn-orphans #-}
module ChaosBox.Geometry.Quad
  ( QuadOf(..)
  , Quad
  , quad
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Geometry.P2
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon      ( polygon )
import           ChaosBox.Geometry.Rect
import           ChaosBox.AABB
import           ChaosBox.Geometry.Class
import           Control.Lens                   ( (&)
                                                , (+~)
                                                )
import           Data.Foldable                  ( for_ )
import           Data.List.NonEmpty             ( NonEmpty(..) )

data QuadOf a = QuadOf
  { quadA :: a
  , quadB :: a
  , quadC :: a
  , quadD :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance HasV2 a => Affine (QuadOf a) where
  transform = defaultTransform

instance HasV2 a => HasAABB (QuadOf a) where
  aabb QuadOf {..} = boundary $ quadA :| [quadB, quadC, quadD]

-- To avoid cyclic dependencies this orphan instance must be located here
instance HasV2 a => Affine (RectOf a) where
  type Transformed (RectOf a) = QuadOf a
  transform m = transform m . fromRect

instance HasV2 a => Draw (QuadOf a) where
  draw QuadOf {..} = for_ (polygon [quadA, quadB, quadC, quadD]) draw

type Quad = QuadOf P2

quad :: a -> a -> a -> a -> QuadOf a
quad = QuadOf

fromRect :: HasV2 a => RectOf a -> QuadOf a
fromRect RectOf {..} = QuadOf rectTopLeft
                              (rectTopLeft & _V2 +~ V2 rectW 0)
                              (rectTopLeft & _V2 +~ V2 rectW rectH)
                              (rectTopLeft & _V2 +~ V2 0 rectH)
