module ChaosBox.Sequence
  ( lerpSeq
  , lerpSeq2
  , lerpSeqWith
  )
where

import           ChaosBox.Math (clamp)
import           Data.Sequence (Seq, index)
import           Linear.V2

-- | Get the element of a sequence some percentage through it
--
-- For a non-empty sequence:
--
-- @lerpSeq 0 seq = head seq@
-- @lerpSeq 1 seq = last seq@
--
lerpSeq :: Double -> Seq a -> a
lerpSeq perc xs = xs `index` lerpSeqIndex perc xs

lerpSeqIndex :: Double -> Seq a -> Int
lerpSeqIndex perc xs = floor $ perc * fromIntegral (length xs - 1)

lerpSeqWith :: (Double -> Double) -> Double -> Seq a -> a
lerpSeqWith f perc xs = lerpSeq (f perc) xs

lerpSeq2 :: V2 Double -> Seq (Seq a) -> a
lerpSeq2 (V2 x0 y0) xs =
  row `index` (floor $ y * fromIntegral (length row - 1))
 where
  row = xs `index` (floor $ x * fromIntegral (length xs - 1))
  x   = clamp (0, 1) x0
  y   = clamp (0, 1) y0
