
{-# LANGUAGE TypeFamilies #-}
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
import qualified ChaosBox.Math.Matrix            as Matrix
import           ChaosBox.Prelude                hiding (point)
import           Control.Lens

import           Data.Foldable                   (for_)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE
import           Graphics.Rendering.Cairo        (Render, arc, closePath,
                                                  lineTo, moveTo, newPath,
                                                  rectangle, setMatrix)
import qualified Graphics.Rendering.Cairo.Matrix as CairoMatrix

-- | A class of items that are transformable via linear transformations
class Affine a where
  matrixLens :: Lens' a (M33 Double)

-- | Reset a transformation matrix to 'identity'
resetMatrix :: Affine a => a -> a
resetMatrix = set matrixLens identity

-- | Apply a function and reset the transformation matrix afterwards.
--
-- This is commonly used for "baking", where the transformation matrix gets
-- applied to the underlying data type directly in haskell values so we don't
-- need to hold onto the matrix anymore.
--
withReset :: (Affine a, Affine b) => (a -> b) -> a -> b
withReset f a = resetMatrix (f a)

-- TODO:
--
-- I should ONLY carry around a transformation matrix on top of the data type
-- and implement 'Affine' for each
--
-- -- can implement some shape sampling to pull points in the shape
--
-- The only problem here is that non-linear transformations must be
-- applied *after* the current transformation has taken place.
--
-- So the translation matrix must be applied directly (in haskell) before any
-- others take place.  The solution here might be to introduce modifying
-- combinators, e.g.
--
-- transformPath :: (V2 Double -> V2 Double) -> Path -> Path
-- transformPath f = map (f . applyMatrix pathMatrix) getPath
--
-- -- basically MonoFunctor -- does it satisfy the functor laws?
-- class Transform t where
--  transform :: (V2 Double -> V2 Double) -> t -> t
--
-- fmap id = id
-- -- breaks this law, since fmap would change all points AND matrix
-- -- _but not_
-- -- if equality is loose (the view of the item)
--
-- fmap (g . f) = fmap g . fmap f
--
-- -- same deal, I think; applying `g` then `f` works fine as long as
-- -- equality == equality by view
-- --
--
-- class Affine t => Transform t where
-- -- ...

-- | An open path
data Path = Path { getPath :: NonEmpty (V2 Double), pathMatrix :: M33 Double }
  deriving (Show, Eq, Ord)

-- | Get a V2 transformation from the 'Affine' transformation
applyAffine :: Affine a => a -> V2 Double -> V2 Double
applyAffine = Matrix.apply . view matrixLens

-- NB. this operation and those like it are useful but should not be a part of
-- the public API. Maybe 'rawPath' should grab the innards after application
-- etc?
bakePath :: Path -> Path
bakePath = withReset $ \p -> p { getPath = fmap (applyAffine p) (getPath p) }

rawPath :: Path -> NonEmpty (V2 Double)
rawPath = getPath . bakePath

instance Affine Path where
  matrixLens wrap (Path p m) = fmap (Path p) (wrap m)

path :: [V2 Double] -> Maybe Path
path xs = Path <$> NE.nonEmpty xs <*> pure identity

instance Draw Path where
  draw (Path (V2 startX startY :| rest) m) = withCairoAffine m $ do
    newPath
    moveTo startX startY
    for_ rest (\(V2 x y) -> lineTo x y)

-- | A closed path
data Polygon = Polygon { getPolygon :: NonEmpty (V2 Double), polygonMatrix :: M33 Double}
  deriving (Show, Eq, Ord)

instance Affine Polygon where
  matrixLens wrap (Polygon p m) = fmap (Polygon p) (wrap m)

polygon :: [V2 Double] -> Maybe Polygon
polygon xs = Polygon <$> NE.nonEmpty xs <*> pure identity

instance Draw Polygon where
  draw (Polygon (V2 startX startY :| rest) m) = withCairoAffine m $ do
    newPath
    moveTo startX startY
    for_ rest (\(V2 x y) -> lineTo x y)
    closePath

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
    $ \p -> draw p { polygonMatrix = quadMatrix }

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
    $ \p -> draw p { polygonMatrix = triangleMatrix }

-- | A circle with diameter 1
point :: V2 Double -> Circle
point center = Circle center 0.5 identity

-- brittany-ignore-next-binding

withCairoAffine :: M33 Double -> Render () -> Render ()
withCairoAffine (V3 (V3 a b c) (V3 d e f) _) render = do
  setMatrix cairoMatrix
  render
  setMatrix CairoMatrix.identity
  where cairoMatrix = CairoMatrix.Matrix a b c d e f
