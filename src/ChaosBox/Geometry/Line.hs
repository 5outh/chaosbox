{-# LANGUAGE ViewPatterns #-}
-- | Line segments
module ChaosBox.Geometry.Line
  ( LineOf(..)
  , Line
  , pattern Line
  , lineStart
  , lineEnd
  , lineCenter
  -- * Transforming 'Line's
  , translateLine
  , scaleLine
  , scaleLineAround
  , rotateLine
  , rotateLineAround
  )
where

import           ChaosBox.AABB
import           ChaosBox.Draw
import           ChaosBox.Geometry.Angle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Path
import           ChaosBox.Geometry.Transform
import           Data.List.NonEmpty
import           Data.Maybe                  (maybeToList)
import           Linear.V2                   (V2 (..))
import           Linear.Vector               ((*^))
import           Control.Lens ((^.))
import           ChaosBox.Math (average)

data LineOf a = LineOf { lineOfStart :: a, lineOfEnd :: a}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Line = LineOf P2

pattern Line :: P2 -> P2 -> Line
pattern Line { lineStart, lineEnd } = LineOf lineStart lineEnd
{-# COMPLETE Line #-}

instance HasP2 a => HasAABB (LineOf a) where
  aabb LineOf {..} = boundary $ lineOfStart :| [lineOfEnd]

instance HasP2 a => Draw (LineOf a) where
  draw LineOf {..} = draw $ PathOf (lineOfStart :| [lineOfEnd])

instance (HasP2 a, HasP2 b) => Intersects (LineOf a) (LineOf b) where
  intersectionPoints a b = maybeToList $ segmentIntersectionPoint a b

segmentIntersectionPoint :: (HasP2 a, HasP2 b) =>  LineOf a -> LineOf b -> Maybe P2
segmentIntersectionPoint (fmap getP2 -> LineOf { lineOfStart = p, lineOfEnd = pr }) (fmap getP2 -> LineOf { lineOfStart = q, lineOfEnd = qs })
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

translateLine :: HasP2 a => P2 -> LineOf a -> LineOf a
translateLine = translatePoints

scaleLine :: HasP2 a => P2 -> LineOf a -> LineOf a
scaleLine = scalePoints

scaleLineAround :: HasP2 a => P2 -> P2 -> LineOf a -> LineOf a
scaleLineAround = scaleAroundPoints

rotateLine :: HasP2 a => Angle -> LineOf a -> LineOf a
rotateLine = rotatePoints

rotateLineAround :: HasP2 a => P2 -> Angle -> LineOf a -> LineOf a
rotateLineAround = rotateAroundPoints

-- | The center of mass of a 'Line'
lineCenter :: HasP2 a => LineOf a -> P2
lineCenter = average . fmap (^._V2)
