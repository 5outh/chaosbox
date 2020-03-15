{-# LANGUAGE ViewPatterns #-}
-- | Line segments
module ChaosBox.Geometry.Line
  ( LineOf(..)
  , Line
  , pattern Line
  )
where

import           ChaosBox.AABB
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Path
import           Data.List.NonEmpty
import           Data.Maybe              (maybeToList)
import           Linear.V2               (V2 (..))
import           Linear.Vector           ((*^))

data LineOf a = LineOf { lineStart :: a, lineEnd :: a}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Line = LineOf P2

pattern Line :: P2 -> P2 -> Line
pattern Line s e = LineOf s e
{-# COMPLETE Line #-}

instance HasP2 a => HasAABB (LineOf a) where
  aabb LineOf {..} = boundary $ lineStart :| [lineEnd]

instance HasP2 a => Affine (LineOf a) where
  transform = defaultTransform

instance HasP2 a => Draw (LineOf a) where
  draw LineOf {..} = draw $ PathOf (lineStart :| [lineEnd])

instance (HasP2 a, HasP2 b) => Intersects (LineOf a) (LineOf b) where
  intersectionPoints a b = maybeToList $ segmentIntersectionPoint a b

segmentIntersectionPoint :: (HasP2 a, HasP2 b) =>  LineOf a -> LineOf b -> Maybe P2
segmentIntersectionPoint (fmap getP2 -> LineOf { lineStart = p, lineEnd = pr }) (fmap getP2 -> LineOf { lineStart = q, lineEnd = qs })
  | r `cross2` s == 0 && (q - p) `cross2` r == 0
  = Nothing
  | -- Collinear; don't worry about the rest of this case.
    r `cross2` s == 0 && (q - p) `cross2` r /= 0
  = Nothing
  | -- Parallel
    r `cross2` s /= 0 && t `inRange` (0, 1) && u `inRange` (0, 1)
  = Just $ q + (u *^ s)
  | otherwise
  = Nothing -- Not intersecting, but not parallel
 where
  r = pr - p
  s = qs - q
  t = (q - p) `cross2` s / (r `cross2` s)
  u = (q - p) `cross2` r / (r `cross2` s)
  inRange x (a, b) = x >= a && x <= b

cross2 :: P2 -> P2 -> Double
V2 vx vy `cross2` V2 wx wy = (vx * wy) - (vy * wx)
