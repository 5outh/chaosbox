{-# LANGUAGE TypeFamilies #-}
module ChaosBox.Geometry.Circle
  ( Circle(..)
  , circle
  , point
  )
where

import           ChaosBox.Prelude        hiding ( point )

import           ChaosBox.Geometry.Polygon
import           ChaosBox.Affine
import           ChaosBox.Draw
import           Graphics.Rendering.Cairo
                                         hiding ( transform )

-- | A circle with radius 'circleRadius' centered at 'circleCenter'
data Circle = Circle { circleCenter :: V2 Double, circleRadius :: Double, circleDetail :: Int }
  deriving (Show, Eq, Ord)

instance Affine Circle where
  type Transformed Circle = Maybe Polygon
  transform m c = case toPolygon c of
    Nothing -> Nothing
    Just p -> Just $ transform m p

instance Draw Circle where
  draw Circle {..} = do
    let V2 x y = circleCenter
    moveTo (x + circleRadius) y
    arc x y circleRadius 0 (2 * pi)

circle :: V2 Double -> Double -> Circle
circle v r = Circle v r 200

-- | A circle with diameter 1
point :: V2 Double -> Circle
point center = Circle center 0.5 200

toPolygon :: Circle -> Maybe Polygon
toPolygon = polygon . circlePoints

circlePoints :: Circle -> [V2 Double]
circlePoints Circle {..} = tail points
 where
  step      = 2 * pi / fromIntegral circleDetail
  intervals = [0, step .. (2 * pi)]
  points    = map ((+ circleCenter) . (^* circleRadius) . angle) intervals
