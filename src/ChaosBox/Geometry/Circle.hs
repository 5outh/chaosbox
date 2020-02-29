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

import           ChaosBox.Prelude        hiding ( point )

import           ChaosBox.Geometry.P2
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon
import           ChaosBox.AABB
import           ChaosBox.Geometry.Class
import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (^.)
                                                )
import           Graphics.Rendering.Cairo
                                         hiding ( transform )

-- | A circle with radius 'circleRadius' centered at 'circleCenter'
data CircleOf a = CircleOf { circleCenter :: a, circleRadius :: Double, circleDetail :: Int }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Circle = CircleOf P2

instance HasP2 a => HasAABB (CircleOf a) where
  aabb CircleOf {..} = boundary $ tl :| [br]
   where
    c  = circleCenter ^. _V2
    tl = c - (circleRadius *^ (-1))
    br = c + (circleRadius *^ 1)

instance HasP2 a => Affine (CircleOf a) where
  type Transformed (CircleOf a) = Maybe (PolygonOf a)
  transform m c = case toPolygon c of
    Nothing -> Nothing
    Just p  -> Just $ transform m p

instance HasP2 a => Draw (CircleOf a) where
  draw CircleOf {..} = do
    let V2 x y = circleCenter ^. _V2
    moveTo (x + circleRadius) y
    arc x y circleRadius 0 (2 * pi)

circle :: a -> Double -> CircleOf a
circle v r = CircleOf v r 200

-- | A circle with diameter 1
point :: a -> CircleOf a
point center = CircleOf center 0.5 200

toPolygon :: HasP2 a => CircleOf a -> Maybe (PolygonOf a)
toPolygon = polygon . circlePoints

circlePoints :: HasP2 a => CircleOf a -> [a]
circlePoints CircleOf {..} = tail $ flip map points $ \v ->
  circleCenter & _V2 .~ v
 where
  step = 2 * pi / fromIntegral circleDetail
  intervals = [0, step .. (2 * pi)]
  points = map ((+ circleCenter ^. _V2) . (^* circleRadius) . angle) intervals

pointsOnCircle :: HasP2 a => Int -> CircleOf a -> [a]
n `pointsOnCircle` c = circlePoints $ c { circleDetail = n }
