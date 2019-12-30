module ChaosBox.Geometry
  ( Path(..)
  , path
  , Polygon(..)
  , polygon
  , Circle(..)
  , Rect(..)
  , Segment(..)
  , Arc(..)
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Draw
import           Data.Foldable                  ( for_ )
import           Data.List.NonEmpty
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
path = fmap Path . nonEmpty

instance Draw Path where
  draw (Path ((V2 startX startY):|rest)) = do
    newPath
    moveTo startX startY
    for_   rest   (\(V2 x y) -> lineTo x y)

-- | A closed path
data Polygon = Polygon { getPolygon :: NonEmpty (V2 Double) }
  deriving (Show, Eq, Ord)

polygon :: [V2 Double] -> Maybe Polygon
polygon = fmap Polygon . nonEmpty

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
