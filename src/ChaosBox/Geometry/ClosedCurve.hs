{-# LANGUAGE ScopedTypeVariables #-}
module ChaosBox.Geometry.ClosedCurve
  ( ClosedCurveOf(..)
  , ClosedCurve
  , closedCurve
  , drawWithDetail
  , fromPolygon
  , toPolygon
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon
import           ChaosBox.HasV2
import           Control.Lens
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           Graphics.Rendering.Cairo  (Render)

-- | Closed Cubic B-Spline
data ClosedCurveOf a = ClosedCurveOf { getClosedCurveOf :: NonEmpty a, closedCurveIterations :: Int }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type ClosedCurve = ClosedCurveOf (V2 Double)

closedCurveOf :: [a] -> Maybe (ClosedCurveOf a)
closedCurveOf xs = ClosedCurveOf <$> NE.nonEmpty xs <*> pure 4

closedCurve :: [V2 Double] -> Maybe ClosedCurve
closedCurve = closedCurveOf

instance HasV2 a => Affine (ClosedCurveOf a) where
  transform = defaultTransform

instance HasV2 a => Draw (ClosedCurveOf a) where
  draw = drawWithDetail

-- | Draw with a specified level of detail (default 5; smaller is less detailed)
drawWithDetail :: HasV2 a => ClosedCurveOf a -> Render ()
drawWithDetail = draw . toPolygon

toPolygon :: HasV2 a => ClosedCurveOf a -> PolygonOf a
toPolygon (ClosedCurveOf ps detail) = PolygonOf newPath
 where
  newPath = NE.fromList $ iterateNLast
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
fromPolygon (PolygonOf p) = ClosedCurveOf p 4

-- TODO: Consolidate
iterateNLast :: Int -> (a -> a) -> a -> a
iterateNLast n f x = last . take n $ iterate f x
