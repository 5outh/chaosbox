{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Quadrilaterals (four-sided polygons)
module ChaosBox.Geometry.Quad
  ( QuadOf(..)
  , Quad
  , pattern Quad
  , quadA
  , quadB
  , quadC
  , quadD
  )
where

import           ChaosBox.Prelude

import           ChaosBox.AABB
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Polygon (polygonOf)
import           ChaosBox.Geometry.Rect
import           Control.Lens              ((&), (+~))
import           Data.Foldable             (for_)
import           Data.List.NonEmpty        (NonEmpty (..))

data QuadOf a = QuadOf
  { quadOfA :: a
  , quadOfB :: a
  , quadOfC :: a
  , quadOfD :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Quad = QuadOf P2

pattern Quad :: P2 -> P2 -> P2 -> P2 -> Quad
pattern Quad {quadA, quadB, quadC, quadD} = QuadOf quadA quadB quadC quadD
{-# COMPLETE Quad #-}

instance HasP2 a => Affine (QuadOf a) where
  transform = defaultTransform

instance HasP2 a => HasAABB (QuadOf a) where
  aabb QuadOf {..} = boundary $ quadOfA :| [quadOfB, quadOfC, quadOfD]

-- To avoid cyclic dependencies this orphan instance must be located here
instance HasP2 a => Affine (RectOf a) where
  type Transformed (RectOf a) = QuadOf a
  transform m = transform m . fromRect

instance HasP2 a => Draw (QuadOf a) where
  draw QuadOf {..} = for_ (polygonOf [quadOfA, quadOfB, quadOfC, quadOfD]) draw

fromRect :: HasP2 a => RectOf a -> QuadOf a
fromRect RectOf {..} = QuadOf rectOfTopLeft
                              (rectOfTopLeft & _V2 +~ V2 rectOfW 0)
                              (rectOfTopLeft & _V2 +~ V2 rectOfW rectOfH)
                              (rectOfTopLeft & _V2 +~ V2 0 rectOfH)
