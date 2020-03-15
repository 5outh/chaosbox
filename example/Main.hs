module Main where

import           ChaosBox

import           ChaosBox.Interactive
import           ChaosBox.Math        (lerp)
import           Control.Monad        (replicateM)
import qualified Data.List.NonEmpty   as NE
import           UnliftIO.IORef

-- Run this example with
--
-- @@@
-- > chaosbox-example -- --scale=60
-- @@@
--
main :: IO ()
main = do
  runChaosBoxWith (\o -> o { optWidth = 10, optHeight = 10, optFps = 30 })
                  renderSketch

renderSketch :: Generate ()
renderSketch = do
  setup

  (w, h)     <- getSize

  center     <- getCenterPoint
  randomPath <- fmap NE.fromList . replicateM 1 $ normal center $ P2 (w / 4)
                                                                     (h / 4)

  pathRef          <- newIORef randomPath
  noise            <- newNoise2

  mousePositionRef <- heldMousePosition ButtonLeft

  debugEvents

  eventLoop $ do
    nextPath <- modifyIORefM pathRef $ \ps@(p NE.:| _) -> do
      c         <- readIORefWith (maybe p (lerp 0.05 p)) mousePositionRef
      nextPoint <- normal c (P2 (noise (c / 100) * 0.3) (noise (c / 100) * 0.3))
      pure $ NE.fromList $ NE.take 400 $ nextPoint `NE.cons` ps

    fillScreenRGB white
    cairo $ do
      setSourceRGB black
      draw (ClosedCurve nextPath 6) *> fill

setup :: Generate ()
setup = do
  fillScreenRGB white
  cairo $ do
    setLineWidth 0.02
    setLineJoin LineJoinRound
    setLineCap LineCapRound
    setSourceRGB black
