module ChaosBox.Geometry.P2
  ( P2
  , pattern P2
  , translateP2
  , scaleP2
  , scaleP2Around
  )
where

import           Linear.V2

-- | A monomorphized 'V2'
type P2 = V2 Double

-- | A @pattern@ to match 'V2's using 'P2' for consistency.
--
-- Construction:
--
-- > a :: P2 = P2 0 0
--
-- Pattern matching:
--
-- > for_ path $ \(P2 x y) -> {- ... -}
--
pattern P2 :: Double -> Double -> P2
pattern P2 x y = V2 x y
{-# COMPLETE P2 #-}

-- | Translate a 'P2' by an offset vector
translateP2 :: P2 -> P2 -> P2
translateP2 offset p2 = p2 - offset

-- | Scale a 'P2' around the origin (@(0,0)@) by a 2d scalar
scaleP2 :: P2 -> P2 -> P2
scaleP2 (P2 x1 y1) (P2 x2 y2) = P2 (x1*x2) (y1*y2)

-- | Scale a 'P2' around the specified point
scaleP2Around
  :: P2
  -- ^ Point to scale around
  -> P2
  -- ^ Scalar
  -> P2
  -- ^ Point to scale
  -> P2
scaleP2Around center amount p2 = translateP2 center (scaleP2 amount (translateP2 (-center) p2))
