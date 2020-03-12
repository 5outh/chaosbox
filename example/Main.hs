module Main where

import           ChaosBox

import           ChaosBox.Math                 (lerp)
import           ChaosBox.Video
import           Control.Monad                 (replicateM)
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Foldable                 (for_)
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
  -- TODO: is there a way for this to be async???
  runChaosBoxInteractive
    (opts { optWidth = 10, optHeight = 10, optScale = 60, optFps = 60 })
    renderSketch

renderSketch :: RandT PureMT (ReaderT GenerateCtx Render) ()
renderSketch = do
  setup

  (w, h)     <- getSize

  center     <- getCenterPoint
  randomPath <- fmap NE.fromList . replicateM 1 $ normal center $ P2 (w / 4)
                                                                     (h / 4)

  pathRef         <- newIORef randomPath
  noise           <- newNoise2

  clickedPointRef <- newIORef Nothing
  onMouseDown $ writeIORef clickedPointRef . Just
  onMouseUp $ \_ -> writeIORef clickedPointRef Nothing
  -- only update when mouse is down
  onMouseMotion $ \p -> do
    mPoint <- readIORef clickedPointRef
    for_ mPoint $ \_ -> writeIORef clickedPointRef (Just p)

  debugEvents

  eventLoop $ do
    ps@(p NE.:| _) <- readIORef pathRef
    clickedPoint   <- readIORef clickedPointRef

    let c = case clickedPoint of
          Nothing -> p
          Just p0 -> lerp 0.05 p p0

    nextPoint <- normal c (P2 (noise (c / 100)) (noise (c / 100)))

    let nextPath = NE.fromList $ NE.take 400 $ nextPoint `NE.cons` ps
    writeIORef pathRef nextPath

    fillScreenRGB white
    cairo $ do
      setSourceRGB black
      draw (Polygon nextPath) *> fill

setup :: Generate ()
setup = do
  fillScreenRGB white
  cairo $ do
    setLineWidth 0.02
    setLineJoin LineJoinRound
    setLineCap LineCapRound
    setSourceRGB black
