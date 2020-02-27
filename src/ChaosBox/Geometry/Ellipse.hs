{-# LANGUAGE TypeFamilies #-}
module ChaosBox.Geometry.Ellipse
  ( EllipseOf(..)
  , Ellipse
  , ellipse
  , ellipsePoints
  )
where

import           ChaosBox.Prelude          hiding (scaled)

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Circle
import           ChaosBox.Geometry.Polygon
import qualified ChaosBox.Geometry.Rect    as Rect
import           ChaosBox.HasAABB
import           ChaosBox.HasV2
import           ChaosBox.Math             (lerpMany)
import qualified ChaosBox.Math.Matrix      as Matrix
import           Control.Lens              (set, (&), (^.))
import           Data.Foldable             (for_)

-- | Axis-bound ellipse
data EllipseOf a = EllipseOf
  { ellipseCenter :: a
  , ellipseWidth  :: Double
  , ellipseHeight :: Double
  , ellipseDetail :: Int
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Ellipse = EllipseOf (V2 Double)

instance HasV2 a => HasAABB (EllipseOf a) where
  aabb EllipseOf {..} = Rect.bounds [tl, br]
   where
    c  = ellipseCenter ^. _V2
    tl = c - V2 ellipseWidth ellipseHeight
    br = c + V2 ellipseWidth ellipseHeight


instance HasV2 a => Affine (EllipseOf a) where
  type Transformed (EllipseOf a) = Maybe (PolygonOf a)
  transform m e = case toPolygon e of
    Nothing -> Nothing
    Just p  -> Just $ transform m p

-- | An ellipse with default detail (200)
ellipse :: a -> Double -> Double -> EllipseOf a
ellipse c w h = EllipseOf c w h 200

instance HasV2 a => Draw (EllipseOf a) where
  draw e = for_ (toPolygon e) draw

-- | Sample 'N' evenly spaced points along the ellipse's path
ellipsePoints :: HasV2 a => EllipseOf a -> [a]
ellipsePoints EllipseOf {..} =
  map ((\p -> ellipseCenter & set _V2 p) . ellipsePoint)
    $ lerpMany ellipseDetail 0 (2 * pi)
 where
  V2 x y = ellipseCenter ^. _V2
  mat    = Matrix.scalar (V2 ellipseWidth ellipseHeight)
    * Matrix.translation (ellipseCenter ^. _V2)
  ellipsePoint t = Matrix.applyMatrix mat $ V2 (x + cos t) (y + sin t)

toPolygon :: HasV2 a => EllipseOf a -> Maybe (PolygonOf a)
toPolygon EllipseOf {..} =
  transform
      (   Matrix.translation (ellipseCenter ^. _V2)
      !*! Matrix.scalar (V2 ellipseWidth ellipseHeight)
      )
    $ circle (ellipseCenter & set _V2 0) 1
