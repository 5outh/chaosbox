module ChaosBox.Geometry
  ( Arc(..)
  , Circle(..)
  , Ellipse(..)
  , Line(..)
  , Path(..)
  , Polygon(..)
  , Quad(..)
  , Rect(..)
  , Triangle(..)
  -- * Smart constructors
  , ellipse
  , path
  , point
  , polygon
  , square
  )
where

import           ChaosBox.Prelude        hiding ( point )

import           ChaosBox.Draw
import           Data.Foldable                  ( for_ )
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Graphics.Rendering.Cairo       ( lineTo
                                                , moveTo
                                                , newPath
                                                , arc
                                                , rectangle
                                                , closePath
                                                )

-- | An open path
data Path = Path { getPath :: NonEmpty (V2 Double) }
  deriving (Show, Eq, Ord)

path :: [V2 Double] -> Maybe Path
path = fmap Path . NE.nonEmpty

instance Draw Path where
  draw (Path ((V2 startX startY):|rest)) = do
    newPath
    moveTo startX startY
    for_   rest   (\(V2 x y) -> lineTo x y)

-- | A closed path
data Polygon = Polygon { getPolygon :: NonEmpty (V2 Double) }
  deriving (Show, Eq, Ord)

polygon :: [V2 Double] -> Maybe Polygon
polygon = fmap Polygon . NE.nonEmpty

instance Draw Polygon where
  draw Polygon{..} = draw (Path getPolygon) *> closePath

-- | A circle with radius 'circleRadius' centered at 'circleCenter'
data Circle = Circle { circleCenter :: V2 Double, circleRadius :: Double }
  deriving (Show, Eq, Ord)

instance Draw Circle where
  draw Circle {..} = do
    let
      V2 x y = circleCenter
    moveTo (x + circleRadius) (y)
    arc x y circleRadius 0 (2 * pi)

-- | A Rectangle
data Rect = Rect
  { rectTopLeft :: V2 Double
  , rectW :: Double
  , rectH :: Double
  } deriving (Show, Eq, Ord)

instance Draw Rect where
  draw Rect {..} = let (V2 rectX rectY) = rectTopLeft in rectangle rectX rectY rectW rectH

square :: V2 Double -> Double -> Rect
square c w = Rect c w w

-- | A line segment
data Line = Line
  { lineStart :: V2 Double
  , lineEnd   :: V2 Double
  } deriving (Show, Eq, Ord)

instance Draw Line where
  draw Line {..} = draw (Path (lineStart:| [lineEnd]))

-- | An Arc (partial Circle)
data Arc = Arc
  { arcCenter :: V2 Double
  -- ^ Center of the arc's circle
  , arcRadius :: Double
  -- ^ Radius of the arc's circle
  , arcStart  :: Double
  -- ^ Start angle in radians
  , arcEnd    :: Double
  -- ^ End angle in radians
  } deriving (Eq, Ord, Show)

instance Draw Arc where
  draw Arc{..} = arc x y arcRadius arcStart arcEnd
   where V2 x y = arcCenter

data Ellipse = Ellipse
  { ellipseCenter :: V2 Double
  , ellipseWidth :: Double
  , ellipseHeight :: Double
  , ellipseDetail :: Int
  }

-- | An ellipse with default detail (100)
ellipse :: V2 Double -> Double -> Double -> Ellipse
ellipse c w h = Ellipse c w h 100

instance Draw Ellipse where
  draw ellipse@Ellipse{..}
    | ellipseDetail <= 0 = error $ "Ellipse detail must be greater than zero. Provided: " <> show ellipseDetail
    | otherwise = for_ (polygon (ellipsePoints ellipse)) draw

ellipsePoints :: Ellipse -> [V2 Double]
ellipsePoints Ellipse {..} = map
  ellipsePoint
  (map unV1 $ lerpMany ellipseDetail (V1 0) (V1 $ 2 * pi))
 where
  V2 x y = ellipseCenter
  ellipsePoint t = V2 (x + ellipseWidth * cos t) (y + ellipseHeight * sin t)
  unV1 (V1 a) = a

data Quad = Quad
  { quadA :: V2 Double
  , quadB :: V2 Double
  , quadC :: V2 Double
  , quadD :: V2 Double
  }

instance Draw Quad where
  draw Quad{..} = for_ (polygon [quadA,quadB,quadC,quadD]) draw

data Triangle = Triangle
  { triangleA :: V2 Double
  , triangleB :: V2 Double
  , triangleC :: V2 Double
  }

instance Draw Triangle where
  draw Triangle{..} = for_ (polygon [triangleA,triangleB,triangleC]) draw

-- | A circle with diameter 1
point :: V2 Double -> Circle
point center = Circle center 0.5
