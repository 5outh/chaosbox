-- | Circles
module ChaosBox.Geometry.Circle
  ( CircleOf(..)
  , Circle
  , pattern Circle
  , circleCenter
  , circleRadius
  , circleDetail
  , circle
  , circleOf
  , point
  , circlePoints
  , pointsOnCircle
  )
where

import           ChaosBox.Prelude          hiding (point)

import           ChaosBox.AABB
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Polygon
import           Control.Lens              ((&), (.~), (^.))
import           Data.List.NonEmpty        (NonEmpty (..))
import           GI.Cairo.Render           hiding (transform)

-- | A circle with radius 'circleOfRadius' centered at 'circleOfCenter'
data CircleOf a = CircleOf { circleOfCenter :: a, circleOfRadius :: Double, circleOfDetail :: Int }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Circle = CircleOf P2

pattern Circle :: P2 -> Double -> Int -> Circle
pattern Circle {circleCenter, circleRadius, circleDetail} = CircleOf circleCenter circleRadius circleDetail
{-# COMPLETE Circle #-}

instance HasP2 a => HasAABB (CircleOf a) where
  aabb CircleOf {..} = boundary $ tl :| [br]
   where
    c  = circleOfCenter ^. _V2
    tl = c - (circleOfRadius *^ (-1))
    br = c + (circleOfRadius *^ 1)

instance HasP2 a => Affine (CircleOf a) where
  type Transformed (CircleOf a) = Maybe (PolygonOf a)
  transform m c = case toPolygon c of
    Nothing -> Nothing
    Just p  -> Just $ transform m p

instance HasP2 a => Draw (CircleOf a) where
  draw CircleOf {..} = do
    let V2 x y = circleOfCenter ^. _V2
    moveTo (x + circleOfRadius) y
    arc x y circleOfRadius 0 (2 * pi)

-- | A 'Circle' with default detail (200)
circleOf :: a -> Double -> CircleOf a
circleOf v r = CircleOf v r 200

circle :: P2 -> Double -> Circle
circle = circleOf @P2

-- | A circle with diameter 1
point :: a -> CircleOf a
point center = CircleOf center 0.5 200

toPolygon :: HasP2 a => CircleOf a -> Maybe (PolygonOf a)
toPolygon = polygonOf . circlePoints

circlePoints :: HasP2 a => CircleOf a -> [a]
circlePoints CircleOf {..} = tail $ flip map points $ \v ->
  circleOfCenter & _V2 .~ v
 where
  step = 2 * pi / fromIntegral circleOfDetail
  intervals = [0, step .. (2 * pi)]
  points = map ((+ circleOfCenter ^. _V2) . (^* circleOfRadius) . angle) intervals

pointsOnCircle :: HasP2 a => Int -> CircleOf a -> [a]
n `pointsOnCircle` c = circlePoints $ c { circleOfDetail = n }
