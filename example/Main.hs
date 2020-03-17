module Main where

import           ChaosBox

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

-- Run this example with
--
-- @
-- > chaosbox-example -- --scale=60
-- @
--
main :: IO ()
main = runChaosBoxWith (\o -> o { optWidth = 10, optHeight = 10 }) renderSketch

setup :: Render ()
setup = setLineWidth 0.02

renderSketch :: Generate ()
renderSketch = do
  cairo setup

  (w, h)           <- getSize
  center           <- getCenterPoint

  startingPoint    <- normal center (P2 (w / 4) (h / 4))
  pathRef          <- newIORef (startingPoint :| [])
  noise            <- newNoise2

  mousePositionRef <- heldMousePosition ButtonLeft

  eventLoop $ do
    nextPath <- modifyIORefM pathRef $ \ps@(p :| _) -> do
      c <- readIORefWith (maybe p (lerp 0.05 p)) mousePositionRef
      let deviation = 0.3 * noise (c / 100)
      nextPoint <- normal c (P2 deviation deviation)
      pure $ unsafeTake 100 $ nextPoint `NE.cons` ps

    fillScreenRGB black
    cairo $ do
      setSourceRGB white
      draw (ClosedCurve nextPath 10) *> stroke

-- | An unsafe version of 'Data.List.NonEmpty.take'
--
-- This will blow up if n < 1, but is perfectly fine for a static value > 1,
-- such as @100@ (at the callsite above).
--
unsafeTake :: Int -> NonEmpty a -> NonEmpty a
unsafeTake n = NE.fromList . NE.take n
