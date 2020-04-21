-- | Axis-aligned ellipses
module ChaosBox.Geometry.Ellipse
  ( EllipseOf(..)
  , Ellipse
  , pattern Ellipse
  , ellipseCenter
  , ellipseWidth
  , ellipseHeight
  , ellipseDetail
  , ellipse
  , ellipseOf
  , ellipsePoints
  , scaleEllipseAround
  , scaleEllipse
  , translateEllipse
  )
where

import           ChaosBox.Prelude            hiding (scaled)

import           ChaosBox.AABB
import           ChaosBox.Draw
import           ChaosBox.Geometry.Circle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Polygon
import           ChaosBox.Geometry.Transform
import           ChaosBox.Math               (lerpMany)
import qualified ChaosBox.Math.Matrix        as Matrix
import           Control.Lens                (set, (&), (^.))
import           Data.Foldable               (for_)
import           Data.List.NonEmpty          (NonEmpty (..))

-- | Axis-bound ellipse
data EllipseOf a = EllipseOf
  { ellipseOfCenter :: a
  , ellipseOfWidth  :: Double
  , ellipseOfHeight :: Double
  , ellipseOfDetail :: Int
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Ellipse = EllipseOf P2

pattern Ellipse :: P2 -> Double -> Double -> Int -> Ellipse
pattern Ellipse {ellipseCenter, ellipseWidth, ellipseHeight, ellipseDetail }
  = EllipseOf ellipseCenter ellipseWidth ellipseHeight ellipseDetail
{-# COMPLETE Ellipse #-}

instance HasP2 a => HasAABB (EllipseOf a) where
  aabb EllipseOf {..} = boundary $ tl :| [ br]
   where
    c  = ellipseOfCenter ^. _V2
    tl = c - V2 ellipseOfWidth ellipseOfHeight
    br = c + V2 ellipseOfWidth ellipseOfHeight

-- | An ellipse with default detail (200) and no rotation
ellipseOf :: a -> Double -> Double -> EllipseOf a
ellipseOf c w h = EllipseOf c w h 200

-- | An ellipse with default detail (200) and no rotation
ellipse :: P2 -> Double -> Double -> Ellipse
ellipse c w h = EllipseOf c w h 200

instance HasP2 a => Draw (EllipseOf a) where
  draw e = for_ (ellipseToPolygon e) draw

-- | Sample 'N' evenly spaced points along the ellipse's path
ellipsePoints :: HasP2 a => EllipseOf a -> [a]
ellipsePoints EllipseOf {..} =
  map ((\p -> ellipseOfCenter & set _V2 p) . ellipsePoint)
    $ lerpMany ellipseOfDetail 0 (2 * pi)
 where
  V2 x y = ellipseOfCenter ^. _V2
  mat    = Matrix.scalar (V2 ellipseOfWidth ellipseOfHeight)
    * Matrix.translation (ellipseOfCenter ^. _V2)
  ellipsePoint t = Matrix.applyMatrix mat $ V2 (x + cos t) (y + sin t)

ellipseToPolygon :: forall a. HasP2 a => EllipseOf a -> Maybe (PolygonOf a)
ellipseToPolygon EllipseOf {..}
  = scalePolygonAround (ellipseOfCenter^._V2) (P2 ellipseOfWidth ellipseOfHeight) <$> mPolygon
 where
  mPolygon :: Maybe (PolygonOf a)
  mPolygon = circleToPolygon
    $ CircleOf ellipseOfCenter 1 ellipseOfDetail

translateEllipse :: HasP2 a => P2 -> EllipseOf a -> EllipseOf a
translateEllipse = translatePoints

scaleEllipse :: HasP2 a => P2 -> EllipseOf a -> EllipseOf a
scaleEllipse (V2 x y) e = e { ellipseOfWidth = ellipseOfWidth e * x, ellipseOfHeight = ellipseOfHeight e * y }

scaleEllipseAround :: HasP2 a => P2 -> P2 -> EllipseOf a -> EllipseOf a
scaleEllipseAround center amount e = scaleEllipse amount (scaleAroundPoints center amount e)
