-- | Closed cubic b-splines
module ChaosBox.Geometry.ClosedCurve
  ( ClosedCurveOf(..)
  , ClosedCurve
  , pattern ClosedCurve
  , getClosedCurve
  , closedCurveIterations
  , closedCurve
  , closedCurveOf
  , closedCurveCenter
  , drawWithDetail
  , fromPolygon
  , toPolygon
  , translateClosedCurve
  , scaleClosedCurve
  , scaleClosedCurveAround
  , rotateClosedCurve
  , rotateClosedCurveAround
  )
where

import           ChaosBox.Math (average)
import           ChaosBox.AABB
import           ChaosBox.Draw
import           ChaosBox.Geometry.Angle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Polygon
import           ChaosBox.Geometry.Transform
import           Control.Lens
import           Data.Foldable
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NE
import           GI.Cairo.Render             (Render)

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

translateClosedCurve :: HasP2 a => P2 -> ClosedCurveOf a -> ClosedCurveOf a
translateClosedCurve = translatePoints

scaleClosedCurve :: HasP2 a => P2 -> ClosedCurveOf a -> ClosedCurveOf a
scaleClosedCurve = scalePoints

scaleClosedCurveAround :: HasP2 a => P2 -> P2 -> ClosedCurveOf a -> ClosedCurveOf a
scaleClosedCurveAround = scaleAroundPoints

rotateClosedCurve :: HasP2 a => Angle -> ClosedCurveOf a -> ClosedCurveOf a
rotateClosedCurve = rotatePoints

rotateClosedCurveAround :: HasP2 a => P2 -> Angle -> ClosedCurveOf a -> ClosedCurveOf a
rotateClosedCurveAround = rotateAroundPoints

-- | The center of mass of a 'ClosedCurve'
closedCurveCenter :: HasP2 a => ClosedCurveOf a -> P2
closedCurveCenter = average . fmap (^._V2)
