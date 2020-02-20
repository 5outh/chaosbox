{-# LANGUAGE NamedFieldPuns #-}
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

import           ChaosBox.Draw
import           ChaosBox.Geometry.Angle
import           ChaosBox.Geometry.Class
import           ChaosBox.Math
import           ChaosBox.Prelude         hiding (point)

import           Data.Foldable            (for_)
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Graphics.Rendering.Cairo (arc, closePath, lineTo, moveTo,
                                           newPath, rectangle)

-- | An open path
newtype Path = Path { getPath :: NonEmpty (V2 Double) }
  deriving (Show, Eq, Ord)

path :: [V2 Double] -> Maybe Path
path = fmap Path . NE.nonEmpty

instance Draw Path where
  draw (Path (V2 startX startY :| rest)) = do
    newPath
    moveTo startX startY
    for_ rest (\(V2 x y) -> lineTo x y)

instance HasCenter Path where
  getCenter (Path ps) = average ps

instance Scale Path where
  scaleAround v s (Path xs) = Path $ fmap (scaleAround v s) xs

instance Translate Path where
  translate v (Path xs) = Path $ fmap (translate v) xs

instance Rotate Path where
  rotateAround v theta (Path xs) = Path $ fmap (rotateAround v theta) xs

-- | A closed path
newtype Polygon = Polygon { getPolygon :: NonEmpty (V2 Double) }
  deriving (Show, Eq, Ord)

polygon :: [V2 Double] -> Maybe Polygon
polygon = fmap Polygon . NE.nonEmpty

instance HasCenter Polygon where
  getCenter (Polygon ps) = average ps

instance Scale Polygon where
  scaleAround v s (Polygon xs) = Polygon $ fmap (scaleAround v s) xs

instance Translate Polygon where
  translate v (Polygon xs) = Polygon $ fmap (translate v) xs

instance Rotate Polygon where
  rotateAround v theta (Polygon xs) = Polygon $ fmap (rotateAround v theta) xs

instance Draw Polygon where
  draw Polygon {..} = draw (Path getPolygon) *> closePath

-- | A circle with radius 'circleRadius' centered at 'circleCenter'
data Circle = Circle { circleCenter :: V2 Double, circleRadius :: Double }
  deriving (Show, Eq, Ord)

instance HasCenter Circle where
  getCenter Circle { circleCenter } = circleCenter

instance Scale Circle where
  -- Note: Maximum of vector scale is used
  scaleAround c s@(V2 sx sy) circle = Circle
    { circleCenter = scaleAround c s (circleCenter circle)
    , circleRadius = circleRadius circle * max sx sy
    }

instance Translate Circle where
  translate v c = c { circleCenter = translate v (circleCenter c) }

instance Rotate Circle where
  rotateAround v theta c =
    c { circleCenter = rotateAround v theta (circleCenter c) }

instance Draw Circle where
  draw Circle {..} = do
    let V2 x y = circleCenter
    moveTo (x + circleRadius) y
    arc x y circleRadius 0 (2 * pi)

-- | A Rectangle
data Rect = Rect
  { rectTopLeft :: V2 Double
  , rectW       :: Double
  , rectH       :: Double
  } deriving (Show, Eq, Ord)

instance Draw Rect where
  draw Rect {..} =
    let (V2 rectX rectY) = rectTopLeft in rectangle rectX rectY rectW rectH

instance HasCenter Rect where
  getCenter Rect {..} = average [rectTopLeft, V2 rectW rectH]

instance Scale Rect where
  scaleAround v s rect@Rect {..} = fromPath (map (scaleAround v s) path)
   where
    path =
      [ rectTopLeft
      , rectTopLeft + V2 rectW 0
      , rectTopLeft + V2 rectW rectH
      , rectTopLeft + V2 0 rectH
      ]
    fromPath [tl, tr, br, bl] = let V2 w h = (br - tl) in Rect tl w h

instance Translate Rect where
  translate v r = r { rectTopLeft = rectTopLeft r + v }

square :: V2 Double -> Double -> Rect
square c w = Rect c w w

-- | A line segment
data Line = Line
  { lineStart :: V2 Double
  , lineEnd   :: V2 Double
  } deriving (Show, Eq, Ord)

instance Draw Line where
  draw Line {..} = draw (Path (lineStart :| [lineEnd]))

instance HasCenter Line where
  getCenter Line {..} = lerpV 0.5 lineStart lineEnd

instance Scale Line where
  scaleAround v s line = line { lineStart = scaleAround v s $ lineStart line
                              , lineEnd   = scaleAround v s $ lineEnd line
                              }

instance Rotate Line where
  rotateAround v theta line = line
    { lineStart = rotateAround v theta $ lineStart line
    , lineEnd   = rotateAround v theta $ lineEnd line
    }

instance Translate Line where
  translate v line = line { lineStart = translate v (lineStart line)
                          , lineEnd   = translate v (lineEnd line)
                          }

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
  } deriving (Eq, Ord, Show)

instance Draw Arc where
  draw Arc {..} = arc x y arcRadius (getAngle arcStart) (getAngle arcEnd)
    where V2 x y = arcCenter

instance Translate Arc where
  translate v a = a { arcCenter = translate v (arcCenter a) }

instance Scale Arc where
  scaleAround v s a = a
    { arcCenter = circleCenter
                    $ scaleAround v s (Circle (arcCenter a) (arcRadius a))
    , arcRadius = circleRadius
                    $ scaleAround v s (Circle (arcCenter a) (arcRadius a))
    }

-- TODO: this one is a bit tricky; use unit vectors of arcStart and arcEnd +
-- arcCenter to get new positions, then derive angles against arcCenter later
--
-- instance Rotate Arc where
  -- rotateAround = ???

data Ellipse = Ellipse
  { ellipseCenter :: V2 Double
  , ellipseWidth  :: Double
  , ellipseHeight :: Double
  , ellipseDetail :: Int
  }

-- | An ellipse with default detail (100)
ellipse :: V2 Double -> Double -> Double -> Ellipse
ellipse c w h = Ellipse c w h 100

instance Draw Ellipse where
  draw ellipse@Ellipse {..}
    | ellipseDetail <= 0
    = error
      $  "Ellipse detail must be greater than zero. Provided: "
      <> show ellipseDetail
    | otherwise
    = for_ (polygon (ellipsePoints ellipse)) draw

ellipsePoints :: Ellipse -> [V2 Double]
ellipsePoints Ellipse {..} = map ellipsePoint
  $ lerpMany ellipseDetail 0 (2 * pi)
 where
  V2 x y = ellipseCenter
  ellipsePoint t = V2 (x + ellipseWidth * cos t) (y + ellipseHeight * sin t)

data Quad = Quad
  { quadA :: V2 Double
  , quadB :: V2 Double
  , quadC :: V2 Double
  , quadD :: V2 Double
  }

instance Draw Quad where
  draw Quad {..} = for_ (polygon [quadA, quadB, quadC, quadD]) draw

data Triangle = Triangle
  { triangleA :: V2 Double
  , triangleB :: V2 Double
  , triangleC :: V2 Double
  }

instance Draw Triangle where
  draw Triangle {..} = for_ (polygon [triangleA, triangleB, triangleC]) draw

-- | A circle with diameter 1
point :: V2 Double -> Circle
point center = Circle center 0.5
