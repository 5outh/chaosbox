module ChaosBox.Geometry.Ellipse
  ( Ellipse(..)
  , ellipse
  , ellipsePoints
  )
where

import           ChaosBox.Prelude

import qualified ChaosBox.Math.Matrix          as Matrix
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Math                  ( lerpMany )
import           ChaosBox.Geometry.Circle

-- | Axis-bound ellipse
data Ellipse = Ellipse
  { ellipseCenter :: V2 Double
  , ellipseWidth  :: Double
  , ellipseHeight :: Double
  , ellipseMatrix :: M33 Double
  }

instance Affine Ellipse where
  matrixLens wrap (Ellipse c w h m) = fmap (Ellipse c w h) (wrap m)

-- | An ellipse with default detail (100)
ellipse :: V2 Double -> Double -> Double -> Ellipse
ellipse c w h = Ellipse c w h identity

instance Draw Ellipse where
  draw Ellipse {..} =
    draw
      $ Circle 0 1
      $ ellipseMatrix
      * Matrix.scalar (V2 ellipseWidth ellipseHeight)
      * Matrix.translation ellipseCenter

-- | Sample 'N' evenly spaced points along the ellipse's path
ellipsePoints :: Int -> Ellipse -> [V2 Double]
ellipsePoints ellipseDetail Ellipse {..} = map ellipsePoint
  $ lerpMany ellipseDetail 0 (2 * pi)
 where
  V2 x y = ellipseCenter
  mat =
    ellipseMatrix
      * Matrix.scalar (V2 ellipseWidth ellipseHeight)
      * Matrix.translation ellipseCenter
  ellipsePoint t = Matrix.apply mat $ V2 (x + cos t) (y + sin t)
