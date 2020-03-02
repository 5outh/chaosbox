{-# LANGUAGE TypeFamilies #-}
module ChaosBox.Geometry.Ellipse
  ( EllipseOf(..)
  , Ellipse
  , pattern Ellipse
  , ellipse
  , ellipseOf
  , ellipsePoints
  )
where

import           ChaosBox.Prelude          hiding (scaled)

import           ChaosBox.AABB
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Circle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Polygon
import           ChaosBox.Math             (lerpMany)
import qualified ChaosBox.Math.Matrix      as Matrix
import           Control.Lens              (set, (&), (^.))
import           Data.Foldable             (for_)
import           Data.List.NonEmpty        (NonEmpty (..))

-- | Axis-bound ellipse
data EllipseOf a = EllipseOf
  { ellipseCenter :: a
  , ellipseWidth  :: Double
  , ellipseHeight :: Double
  , ellipseDetail :: Int
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Ellipse = EllipseOf P2

pattern Ellipse :: P2 -> Double -> Double -> Int -> Ellipse
pattern Ellipse c w h d = EllipseOf c w h d

instance HasP2 a => HasAABB (EllipseOf a) where
  aabb EllipseOf {..} = boundary $ tl :| [ br]
   where
    c  = ellipseCenter ^. _V2
    tl = c - V2 ellipseWidth ellipseHeight
    br = c + V2 ellipseWidth ellipseHeight


instance HasP2 a => Affine (EllipseOf a) where
  type Transformed (EllipseOf a) = Maybe (PolygonOf a)
  transform m e = case toPolygon e of
    Nothing -> Nothing
    Just p  -> Just $ transform m p

-- | An ellipse with default detail (200)
ellipseOf :: a -> Double -> Double -> EllipseOf a
ellipseOf c w h = EllipseOf c w h 200

-- | An ellipse with default detail (200)
ellipse :: P2 -> Double -> Double -> Ellipse
ellipse c w h = EllipseOf c w h 200

instance HasP2 a => Draw (EllipseOf a) where
  draw e = for_ (toPolygon e) draw

-- | Sample 'N' evenly spaced points along the ellipse's path
ellipsePoints :: HasP2 a => EllipseOf a -> [a]
ellipsePoints EllipseOf {..} =
  map ((\p -> ellipseCenter & set _V2 p) . ellipsePoint)
    $ lerpMany ellipseDetail 0 (2 * pi)
 where
  V2 x y = ellipseCenter ^. _V2
  mat    = Matrix.scalar (V2 ellipseWidth ellipseHeight)
    * Matrix.translation (ellipseCenter ^. _V2)
  ellipsePoint t = Matrix.applyMatrix mat $ V2 (x + cos t) (y + sin t)

toPolygon :: HasP2 a => EllipseOf a -> Maybe (PolygonOf a)
toPolygon EllipseOf {..} =
  transform
      (  translated (ellipseCenter ^. _V2)
      <> scaled (V2 ellipseWidth ellipseHeight)
      )
    $ circleOf (ellipseCenter & set _V2 0) 1
