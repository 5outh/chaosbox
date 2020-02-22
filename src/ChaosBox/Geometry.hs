
{-# LANGUAGE TypeFamilies #-}
module ChaosBox.Geometry
  ( Arc(..)
  , Circle(..)
  , Ellipse(..)
  , Line(..)
  , Path(..)
  , Quad(..)
  , Rect(..)
  , Triangle(..)
  -- * Smart constructors
  , ellipse
  , point
  , square
  -- * Combinators
  , ellipsePoints
  , module ChaosBox.Geometry.Path
  , module ChaosBox.Geometry.Polygon
  )
where

import           Control.Lens
import           ChaosBox.Draw
import           ChaosBox.Geometry.Angle
import           ChaosBox.Math
import qualified ChaosBox.Math.Matrix          as Matrix
import           ChaosBox.Prelude        hiding ( point )

import           Data.Foldable                  ( for_ )
import           Data.List.NonEmpty             ( NonEmpty(..) )
-- import qualified Data.List.NonEmpty            as NE
import           Graphics.Rendering.Cairo       ( arc
                                                , moveTo
                                                , rectangle
                                                )
import           ChaosBox.Affine
import           ChaosBox.Geometry.Path
import           ChaosBox.Geometry.Polygon

-- | A circle with radius 'circleRadius' centered at 'circleCenter'
data Circle = Circle { circleCenter :: V2 Double, circleRadius :: Double, circleMatrix :: M33 Double }
  deriving (Show, Eq, Ord)

instance Affine Circle where
  matrixLens wrap (Circle c r m) = fmap (Circle c r) (wrap m)

instance Draw Circle where
  draw Circle {..} = withCairoAffine circleMatrix $ do
    let V2 x y = circleCenter
    moveTo (x + circleRadius) y
    arc x y circleRadius 0 (2 * pi)

-- | A Rectangle
data Rect = Rect
  { rectTopLeft :: V2 Double
  , rectW       :: Double
  , rectH       :: Double
  , rectMatrix  :: M33 Double
  } deriving (Show, Eq, Ord)

instance Affine Rect where
  matrixLens wrap (Rect tl w h m) = fmap (Rect tl w h) (wrap m)

instance Draw Rect where
  draw Rect {..} = withCairoAffine rectMatrix
    $ rectangle rectX rectY rectW rectH
    where V2 rectX rectY = rectTopLeft

-- instance HasCenter Rect where
  -- getCenter Rect {..} = average [rectTopLeft, V2 rectW rectH]

square :: V2 Double -> Double -> Rect
square c w = Rect c w w identity

-- | A line segment
data Line = Line
  { lineStart  :: V2 Double
  , lineEnd    :: V2 Double
  , lineMatrix :: M33 Double
  } deriving (Show, Eq, Ord)

instance Affine Line where
  matrixLens wrap (Line s e m) = fmap (Line s e) (wrap m)

instance Draw Line where
  draw Line {..} = draw $ Path (lineStart :| [lineEnd]) lineMatrix

-- | An Arc (partial Circle)
data Arc = Arc
  { arcCenter :: V2 Double
  -- ^ Center of the arc's circle
  , arcRadius :: Double
  -- ^ Radius of the arc's circle
  , arcStart  :: Angle
  -- ^ Start 'Angle'
  , arcEnd    :: Angle
  -- ^ End 'Angle'
  , arcMatrix :: M33 Double
  } deriving (Eq, Ord, Show)

instance Draw Arc where
  draw Arc {..} = withCairoAffine arcMatrix
    $ arc x y arcRadius (getAngle arcStart) (getAngle arcEnd)
    where V2 x y = arcCenter

  -- TODO: Carry a matrix around with every shape.
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
  ellipsePoint t = Matrix.apply ellipseMatrix
    $ V2 (x + ellipseWidth * cos t) (y + ellipseHeight * sin t)

data Quad = Quad
  { quadA      :: V2 Double
  , quadB      :: V2 Double
  , quadC      :: V2 Double
  , quadD      :: V2 Double
  , quadMatrix :: M33 Double
  }

instance Affine Quad where
  matrixLens wrap (Quad a b c d m) = fmap (Quad a b c d) (wrap m)

instance Draw Quad where
  draw Quad {..} = for_ (polygon [quadA, quadB, quadC, quadD])
    $ draw . set matrixLens quadMatrix

data Triangle = Triangle
  { triangleA      :: V2 Double
  , triangleB      :: V2 Double
  , triangleC      :: V2 Double
  , triangleMatrix :: M33 Double
  }

instance Affine Triangle where
  matrixLens wrap (Triangle a b c m) = fmap (Triangle a b c) (wrap m)

instance Draw Triangle where
  draw Triangle {..} = for_ (polygon [triangleA, triangleB, triangleC])
    $ draw . set matrixLens triangleMatrix

-- | A circle with diameter 1
point :: V2 Double -> Circle
point center = Circle center 0.5 identity
