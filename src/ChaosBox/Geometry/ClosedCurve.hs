-- | Closed cubic b-splines
module ChaosBox.Geometry.ClosedCurve
  ( ClosedCurveOf(..)
  , ClosedCurve
  , pattern ClosedCurve
  , getClosedCurve
  , closedCurveIterations
  , closedCurve
  , closedCurveOf
  , drawWithDetail
  , fromPolygon
  , toPolygon
  )
where

import           ChaosBox.AABB
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Polygon
import           Control.Lens
import           Data.Foldable
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           GI.Cairo.Render           (Render)
import           Linear                    ((*^))

-- | Closed Cubic B-Spline
data ClosedCurveOf a = ClosedCurveOf { getClosedCurveOf :: NonEmpty a, closedCurveOfIterations :: Int }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type ClosedCurve = ClosedCurveOf P2

pattern ClosedCurve :: NonEmpty P2 -> Int -> ClosedCurve
pattern ClosedCurve {getClosedCurve, closedCurveIterations} = ClosedCurveOf getClosedCurve closedCurveIterations
{-# COMPLETE ClosedCurve #-}

closedCurveOf :: [a] -> Maybe (ClosedCurveOf a)
closedCurveOf xs = ClosedCurveOf <$> NE.nonEmpty xs <*> pure 5

closedCurve :: [P2] -> Maybe ClosedCurve
closedCurve = closedCurveOf @P2

instance HasP2 a => HasAABB (ClosedCurveOf a) where
  aabb = boundary .getClosedCurveOf

instance HasP2 a => Affine (ClosedCurveOf a) where
  transform = defaultTransform

instance HasP2 a => Draw (ClosedCurveOf a) where
  draw = drawWithDetail

-- | Draw with a specified level of detail (default 5; smaller is less detailed)
drawWithDetail :: HasP2 a => ClosedCurveOf a -> Render ()
drawWithDetail c = for_ (toPolygon c) draw

toPolygon :: HasP2 a => ClosedCurveOf a -> Maybe (PolygonOf a)
toPolygon (ClosedCurveOf ps detail)
  = fmap PolygonOf
    . NE.nonEmpty
    $ newPath
 where
  newPath = iterateNLast
    detail
    (go . expand)
    (NE.last ps : (NE.toList ps <> NE.take 2 (NE.cycle ps)))

  expand1 prev a = [prev & _V2 .~ (prev ^. _V2 + a ^. _V2) / 2, a]

  expand ys@(y : _) = y : concat (zipWith expand1 ys (tail ys))
  expand []         = error "impossible"

  mask a b c = b & _V2 .~ ((a ^. _V2 + 2 * b ^. _V2 + c ^. _V2) / 4)

  go (a : b : c : xs) = mask a b c : go (b : c : xs)
  go _                = []

fromPolygon :: PolygonOf a -> ClosedCurveOf a
fromPolygon (PolygonOf p) = ClosedCurveOf p 5

-- TODO: Consolidate
iterateNLast :: Int -> (a -> a) -> a -> a
iterateNLast n f x = last . take n $ iterate f x

quads (a:xs@(b:c:d:_)) = (a,b,c,d):quads xs
quads _                = []

catmullRom :: Double -> P2 -> P2 -> P2 -> P2 -> P2
catmullRom t p_1 p0 p1 p2 =
           (t*((2-t)*t - 1)   *^ p_1
         + (t*t*(3*t - 5) + 2) *^ p0
         + t*((4 - 3*t)*t + 1) *^ p1
         + (t-1)*t*t         *^ p2 ) / 2
