module ChaosBox.Geometry.Curve
  ( CurveOf(..)
  , Curve
  , pattern Curve
  , curve
  , curveOf
  , curveWithDetail
  , toPath
  , fromPath
  )
where

import           ChaosBox.AABB
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Path
import           Control.Lens
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Graphics.Rendering.Cairo (Render)

-- | Cubic B-Spline
data CurveOf a = CurveOf { getCurve :: NonEmpty a, curveIterations :: Int }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Curve = CurveOf P2

pattern Curve :: NonEmpty P2 -> Int -> Curve
pattern Curve a i = CurveOf a i

instance HasP2 a => HasAABB (CurveOf a) where
  aabb = aabb . toPath

instance HasP2 a => Affine (CurveOf a) where
  transform = defaultTransform

instance HasP2 a => Draw (CurveOf a) where
  draw = drawWithDetail

-- | Draw with a specified level of detail (default 5; smaller is less detailed)
drawWithDetail :: HasP2 a => CurveOf a -> Render ()
drawWithDetail = draw . toPath

toPath :: HasP2 a => CurveOf a -> PathOf a
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

curveOf :: [a] -> Maybe (CurveOf a)
curveOf xs = CurveOf <$> NE.nonEmpty xs <*> pure 5

curve :: [P2] -> Maybe Curve
curve = curveOf @P2

curveWithDetail :: [a] -> Int -> Maybe (CurveOf a)
curveWithDetail xs detail = CurveOf <$> NE.nonEmpty xs <*> pure detail
