module ChaosBox.Geometry.Curve
  ( CurveOf(..)
  , Curve
  , curve
  , curveWithDetail
  , toPath
  , fromPath
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Path
import           ChaosBox.AABB
import           ChaosBox.Geometry.Class
import           Control.Lens
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Graphics.Rendering.Cairo       ( Render )

-- | Cubic B-Spline
data CurveOf a = CurveOf { getCurve :: NonEmpty a, curveIterations :: Int }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Curve = CurveOf (V2 Double)

instance HasV2 a => HasAABB (CurveOf a) where
  aabb = aabb . toPath

instance HasV2 a => Affine (CurveOf a) where
  transform = defaultTransform

instance HasV2 a => Draw (CurveOf a) where
  draw = drawWithDetail

-- | Draw with a specified level of detail (default 5; smaller is less detailed)
drawWithDetail :: HasV2 a => CurveOf a -> Render ()
drawWithDetail = draw . toPath

toPath :: HasV2 a => CurveOf a -> PathOf a
toPath (CurveOf ps detail) = PathOf
  (NE.fromList $ iterateNLast detail (go . expand) (NE.toList ps))
 where
  expand1 prev a = [prev & _V2 .~ (prev ^. _V2 + a ^. _V2) / 2, a]

  expand ys@(y : _) = y : concat (zipWith expand1 ys (tail ys))
  expand []         = error "impossible"

  mask a b c = b & _V2 .~ (a ^. _V2 + 2 * b ^. _V2 + c ^. _V2) / 4

  go1 []               = []
  go1 [c]              = [c]
  go1 [_, c]           = [c]
  go1 (a : b : c : xs) = mask a b c : go1 (b : c : xs)

  go []         = []
  go xs@(a : _) = a : go1 xs

fromPath :: PathOf a -> CurveOf a
fromPath (PathOf p) = CurveOf p 5

iterateNLast :: Int -> (a -> a) -> a -> a
iterateNLast n f x = last . take n $ iterate f x

curve :: [a] -> Maybe (CurveOf a)
curve xs = CurveOf <$> NE.nonEmpty xs <*> pure 5

curveWithDetail :: [a] -> Int -> Maybe (CurveOf a)
curveWithDetail xs detail = CurveOf <$> NE.nonEmpty xs <*> pure detail
