module Main where

import           ChaosBox

import           ChaosBox.Math                 (lerp)
import           ChaosBox.Video
import           Control.Monad                 (replicateM)
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.IORef.Lifted
import qualified Data.List.NonEmpty            as NE
import           System.Random.Mersenne.Pure64

-- Run this example with
--
-- @@@
-- > chaosbox-example -- --scale=100
-- @@@
--
main :: IO ()
main = do
  opts <- getDefaultOpts
  runChaosBoxWith
    (\o -> o { optWidth = 10, optHeight = 10, optScale = 60, optFps = 30 })
    renderSketch

renderSketch :: RandT PureMT (ReaderT GenerateCtx Render) ()
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
