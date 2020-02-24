{-# LANGUAGE TypeFamilies #-}
module ChaosBox.Geometry.Circle
  ( CircleOf(..)
  , Circle
  , circle
  , point
  , circlePoints
  , pointsOnCircle
  )
where

import           ChaosBox.Prelude          hiding (point)

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon
import           ChaosBox.HasV2
import           Control.Lens              ((&), (.~), (^.))
import           Graphics.Rendering.Cairo  hiding (transform)

-- | A circle with radius 'circleRadius' centered at 'circleCenter'
data CircleOf a = CircleOf { circleCenter :: a, circleRadius :: Double, circleDetail :: Int }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Circle = CircleOf (V2 Double)

instance HasV2 a => Affine (CircleOf a) where
  type Transformed (CircleOf a) = Maybe (PolygonOf a)
  transform m c = case toPolygon c of
    Nothing -> Nothing
    Just p  -> Just $ transform m p

instance HasV2 a => Draw (CircleOf a) where
  draw CircleOf {..} = do
    let V2 x y = circleCenter ^. _V2
    moveTo (x + circleRadius) y
    arc x y circleRadius 0 (2 * pi)

circle :: a -> Double -> CircleOf a
circle v r = CircleOf v r 200

-- | A circle with diameter 1
point :: a -> CircleOf a
point center = CircleOf center 0.5 200

toPolygon :: HasV2 a => CircleOf a -> Maybe (PolygonOf a)
toPolygon = polygon . circlePoints

circlePoints :: HasV2 a => CircleOf a -> [a]
circlePoints CircleOf {..} = tail $ flip map points $ \v ->
  circleCenter & _V2 .~ v
 where
  step = 2 * pi / fromIntegral circleDetail
  intervals = [0, step .. (2 * pi)]
  points = map ((+ circleCenter ^. _V2) . (^* circleRadius) . angle) intervals

pointsOnCircle :: HasV2 a => Int -> CircleOf a -> [a]
n `pointsOnCircle` c = circlePoints $ c { circleDetail = n }
