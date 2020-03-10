module ChaosBox.Geometry.P2
  ( P2
  , pattern P2
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
