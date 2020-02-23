module ChaosBox.Geometry.ClosedCurve
  ( ClosedCurve(..)
  , closedCurve
  , drawWithDetail
  , fromPolygon
  , toPolygon
  , bakeClosedCurve
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           Graphics.Rendering.Cairo  (Render)

-- | Closed Cubic B-Spline
data ClosedCurve = ClosedCurve { getClosedCurve :: NonEmpty (V2 Double), closedCurveMatrix :: M33 Double }
  deriving (Show, Eq, Ord)

closedCurve :: [V2 Double] -> Maybe ClosedCurve
closedCurve xs = flip fmap (NE.nonEmpty xs) $ flip ClosedCurve identity

instance Affine ClosedCurve where
  matrixLens wrap (ClosedCurve p m) = fmap (ClosedCurve p) (wrap m)

instance Draw ClosedCurve where
  draw = drawWithDetail 5

-- | Draw with a specified level of detail (default 5; smaller is less detailed)
drawWithDetail :: Int -> ClosedCurve -> Render ()
drawWithDetail detail = draw . toPolygon detail

toPolygon :: Int -> ClosedCurve -> Polygon
toPolygon detail (ClosedCurve ps m) = Polygon newPath m
 where
  -- pathList = NE.toList ps
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
fromPolygon (Polygon p m) = ClosedCurve p m

iterateNLast :: Int -> (a -> a) -> a -> a
iterateNLast n f x = last . take n $ iterate f x

bakeClosedCurve :: ClosedCurve -> ClosedCurve
bakeClosedCurve c@(ClosedCurve ps _) =
  ClosedCurve (fmap (applyAffine c) ps) identity
