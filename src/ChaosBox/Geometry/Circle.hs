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
  , circleToPolygon
  , translateCircle
  , scaleCircle
  , scaleCircleAround
  , rotateCircle
  , rotateCircleAround
  )
where

import           ChaosBox.Prelude            hiding (point)

import           ChaosBox.AABB
import           ChaosBox.Draw
import           ChaosBox.Geometry.Angle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Polygon
import           ChaosBox.Geometry.Transform
import           Control.Lens                ((&), (.~), (^.))
import           Data.List.NonEmpty          (NonEmpty (..))
import           GI.Cairo.Render             hiding (transform)

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

circleToPolygon :: HasP2 a => CircleOf a -> Maybe (PolygonOf a)
circleToPolygon = polygonOf . circlePoints

circlePoints :: HasP2 a => CircleOf a -> [a]
circlePoints CircleOf {..} = tail $ flip map points $ \v ->
  circleOfCenter & _V2 .~ v
 where
  step = 2 * pi / fromIntegral circleOfDetail
  intervals = [0, step .. (2 * pi)]
  points = map ((+ circleOfCenter ^. _V2) . (^* circleOfRadius) . angle) intervals

pointsOnCircle :: HasP2 a => Int -> CircleOf a -> [a]
n `pointsOnCircle` c = circlePoints $ c { circleOfDetail = n }

translateCircle :: HasP2 a => P2 -> CircleOf a -> CircleOf a
translateCircle = translatePoints

scaleCircle :: HasP2 a => Double -> CircleOf a -> CircleOf a
scaleCircle amount c = c { circleOfRadius = circleOfRadius c * amount }

scaleCircleAround :: HasP2 a => P2 -> Double -> CircleOf a -> CircleOf a
scaleCircleAround center amount c = scaleCircle amount (scaleAroundPoints center (P2 amount amount) c)

rotateCircle :: HasP2 a => Angle -> CircleOf a -> CircleOf a
rotateCircle = rotatePoints

rotateCircleAround :: HasP2 a => P2 -> Angle -> CircleOf a -> CircleOf a
rotateCircleAround = rotateAroundPoints
