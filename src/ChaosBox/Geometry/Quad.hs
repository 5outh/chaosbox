{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Quadrilaterals (four-sided polygons)
module ChaosBox.Geometry.Quad
  ( QuadOf(..)
  , Quad
  , pattern Quad
  , fromRect
  , quadA
  , quadB
  , quadC
  , quadD
  , translateQuad
  , scaleQuad
  , scaleQuadAround
  , rotateQuad
  , rotateQuadAround
  , quadCenter
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Math (average)
import           Control.Lens ((^.))
import           ChaosBox.AABB
import           ChaosBox.Draw
import           ChaosBox.Geometry.Angle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Polygon   (polygonOf)
import           ChaosBox.Geometry.Rect
import           ChaosBox.Geometry.Transform
import           Control.Lens                ((&), (+~))
import           Data.Foldable               (for_)
import           Data.List.NonEmpty          (NonEmpty (..))

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

instance HasP2 a => HasAABB (QuadOf a) where
  aabb QuadOf {..} = boundary $ quadOfA :| [quadOfB, quadOfC, quadOfD]

instance HasP2 a => Draw (QuadOf a) where
  draw QuadOf {..} = for_ (polygonOf [quadOfA, quadOfB, quadOfC, quadOfD]) draw

fromRect :: HasP2 a => RectOf a -> QuadOf a
fromRect RectOf {..} = QuadOf rectOfTopLeft
                              (rectOfTopLeft & _V2 +~ V2 rectOfW 0)
                              (rectOfTopLeft & _V2 +~ V2 rectOfW rectOfH)
                              (rectOfTopLeft & _V2 +~ V2 0 rectOfH)

translateQuad :: HasP2 a => P2 -> QuadOf a -> QuadOf a
translateQuad = translatePoints

scaleQuad :: HasP2 a => P2 -> QuadOf a -> QuadOf a
scaleQuad = scalePoints

scaleQuadAround :: HasP2 a => P2 -> P2 -> QuadOf a -> QuadOf a
scaleQuadAround = scaleAroundPoints

rotateQuad :: HasP2 a => Angle -> QuadOf a -> QuadOf a
rotateQuad = rotatePoints

rotateQuadAround :: HasP2 a => P2 -> Angle -> QuadOf a -> QuadOf a
rotateQuadAround = rotateAroundPoints

-- | The center of mass of a 'Quad'
quadCenter :: HasP2 a => QuadOf a -> P2
quadCenter = average . fmap (^._V2)
