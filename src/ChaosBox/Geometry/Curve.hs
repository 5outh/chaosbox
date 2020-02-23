module ChaosBox.Geometry.Curve
  ( Curve(..)
  , curve
  , toPath
  , fromPath
  , bakeCurve
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Path
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty       as NE
import           Graphics.Rendering.Cairo (Render)

-- | Cubic B-Spline
data Curve = Curve { getCurve :: NonEmpty (V2 Double), curveMatrix :: M33 Double }
  deriving (Show, Eq, Ord)

curve :: [V2 Double] -> Maybe Curve
curve xs = flip fmap (NE.nonEmpty xs) $ flip Curve identity

instance Affine Curve where
  matrixLens wrap (Curve p m) = fmap (Curve p) (wrap m)

instance Draw Curve where
  draw = drawWithDetail 5

-- | Draw with a specified level of detail (default 5; smaller is less detailed)
drawWithDetail :: Int -> Curve -> Render ()
drawWithDetail detail = draw . toPath detail

toPath :: Int -> Curve -> Path
toPath detail (Curve ps m) = Path
  (NE.fromList $ iterateNLast detail (go . expand) (NE.toList ps))
  m
 where
  expand1 prev a = [(prev + a) / 2, a]
  expand ys@(y : _) = y : concat (zipWith expand1 ys (tail ys))
  expand []         = error "impossible"

  mask a b c = (a + 2 * b + c) / 4 -- (Pi-1k-1 + 2 Pik-1 + Pi+1k-1)/4

  go1 []               = []
  go1 [c]              = [c]
  go1 [_, c]           = [c]
  go1 (a : b : c : xs) = mask a b c : go1 (b : c : xs)

  go []         = []
  go xs@(a : _) = a : go1 xs

fromPath :: Path -> Curve
fromPath (Path p m) = Curve p m

iterateNLast :: Int -> (a -> a) -> a -> a
iterateNLast n f x = last . take n $ iterate f x

bakeCurve :: Curve -> Curve
bakeCurve c@(Curve ps _) = Curve (fmap (applyAffine c) ps) identity
