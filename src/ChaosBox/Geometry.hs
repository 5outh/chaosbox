module ChaosBox.Geometry
  ( Path(..)
  , Polygon(..)
  , Circle(..)
  , Rect(..)
  , Segment(..)
  , Arc(..)
  , Ellipse(..)
  -- * Smart constructors
  , path
  , polygon
  )
where

import           ChaosBox.Prelude

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
  { rectX :: Double
  , rectY :: Double
  , rectW :: Double
  , rectH :: Double
  } deriving (Show, Eq, Ord)

instance Draw Rect where
  draw Rect {..} = rectangle rectX rectY rectW rectH

-- | A line segment
data Segment = Segment
  { segmentStart :: V2 Double
  , segmentEnd   :: V2 Double
  } deriving (Show, Eq, Ord)

instance Draw Segment where
  draw Segment {..} = draw (Path (segmentStart:| [segmentEnd]))

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
  ellipsePoint t = V2 (ellipseWidth * cos t) (ellipseHeight * sin t)
  unV1 (V1 a) = a
