module Main where

import           ChaosBox

import ChaosBox.Video
import           ChaosBox.Event
import           ChaosBox.Math                 (lerp)
import           Control.Monad                 (replicateM)
import           Control.Monad.Loops
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.IORef
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                    (fromMaybe)
import qualified SDL
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

  pathRef <- liftIO $ newIORef randomPath
  noise   <- newNoise2

  renderLoop $ do
    ps@(p NE.:| _) <- liftIO $ readIORef pathRef

    mPoint         <- onClick $ pure . lerp 0.05 p

    let c = fromMaybe p mPoint
    next <- normal c 0.1

    let newPath = NE.fromList $ NE.take 400 $ next `NE.cons` ps
    liftIO $ writeIORef pathRef newPath

    fillScreenRGB white
    cairo $ do
      setSourceRGB black
      draw (PolygonOf newPath) *> fill

setup :: Generate ()
setup = do
  cairo $ do
    setLineWidth 0.02
    setLineJoin LineJoinRound
    setLineCap LineCapRound

  fillScreenRGB white

  cairo $ setSourceRGB black
