module ChaosBox.Geometry.ClosedCurve
  ( ClosedCurve(..)
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
import           ChaosBox.Math.Matrix      (applyMatrix)
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           Graphics.Rendering.Cairo  (Render)

-- | Closed Cubic B-Spline
data ClosedCurve = ClosedCurve { getClosedCurve :: NonEmpty (V2 Double) }
  deriving (Show, Eq, Ord)

closedCurve :: [V2 Double] -> Maybe ClosedCurve
closedCurve xs = ClosedCurve <$> NE.nonEmpty xs

instance Affine ClosedCurve where
  transform m = ClosedCurve . fmap (applyMatrix m) . getClosedCurve

instance Draw ClosedCurve where
  draw = drawWithDetail 5

-- | Draw with a specified level of detail (default 5; smaller is less detailed)
drawWithDetail :: Int -> ClosedCurve -> Render ()
drawWithDetail detail = draw . toPolygon detail

toPolygon :: Int -> ClosedCurve -> Polygon
toPolygon detail (ClosedCurve ps) = Polygon newPath
 where
  newPath =
    (NE.fromList $ iterateNLast
      detail
      (go . expand)
      (NE.last ps : (NE.toList ps <> NE.take 2 (NE.cycle ps)))
    )

  expand1 prev a = [(prev + a) / 2, a]
  expand ys@(y : _) = y : concat (zipWith expand1 ys (tail ys))
  expand []         = error "impossible"

  mask a b c = (a + 2 * b + c) / 4 -- (Pi-1k-1 + 2 Pik-1 + Pi+1k-1)/4

  go (a : b : c : xs) = mask a b c : go (b : c : xs)
  go _                = []

fromPolygon :: Polygon -> ClosedCurve
fromPolygon (Polygon p) = ClosedCurve p

-- TODO: Consolidate
iterateNLast :: Int -> (a -> a) -> a -> a
iterateNLast n f x = last . take n $ iterate f x
