{-# LANGUAGE TypeFamilies #-}
module ChaosBox.Geometry.Ellipse
  ( Ellipse(..)
  , ellipse
  , ellipsePoints
  )
where

import           ChaosBox.Prelude        hiding ( scaled )

import           Data.Foldable                  ( for_ )
import qualified ChaosBox.Math.Matrix          as Matrix
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon
import           ChaosBox.Math                  ( lerpMany )
import           ChaosBox.Geometry.Circle

-- | Axis-bound ellipse
data Ellipse = Ellipse
  { ellipseCenter :: V2 Double
  , ellipseWidth  :: Double
  , ellipseHeight :: Double
  , ellipseDetail :: Int
  }

instance Affine Ellipse where
  type Transformed Ellipse = Maybe Polygon
  transform m e = case toPolygon e of
    Nothing -> Nothing
    Just p -> Just $ transform m p

-- | An ellipse with default detail (200)
ellipse :: V2 Double -> Double -> Double -> Ellipse
ellipse c w h = Ellipse c w h 200

instance Draw Ellipse where
  draw e = for_ (toPolygon e) draw

-- | Sample 'N' evenly spaced points along the ellipse's path
ellipsePoints :: Ellipse -> [V2 Double]
ellipsePoints Ellipse {..} = map ellipsePoint
  $ lerpMany ellipseDetail 0 (2 * pi)
 where
  V2 x y = ellipseCenter
  mat    = Matrix.scalar (V2 ellipseWidth ellipseHeight)
    * Matrix.translation ellipseCenter
  ellipsePoint t = Matrix.applyMatrix mat $ V2 (x + cos t) (y + sin t)

toPolygon :: Ellipse -> Maybe Polygon
toPolygon Ellipse {..} =
  transform
      (   Matrix.translation ellipseCenter
      !*! Matrix.scalar (V2 ellipseWidth ellipseHeight)
      )
    $ circle 0 1
