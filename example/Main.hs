module Main where

import           ChaosBox

import           ChaosBox.Math                 (lerp)
import           ChaosBox.Video
import           Control.Lens
import           Control.Monad                 (replicateM)
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.IORef
import qualified Data.List.NonEmpty            as NE
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
    (opts { optWidth = 10, optHeight = 10, optScale = 60 })
    renderSketch

renderSketch :: RandT PureMT (ReaderT GenerateCtx Render) ()
renderSketch = do
  setup

  (w, h)     <- getSize

  center     <- getCenterPoint
  randomPath <- fmap NE.fromList . replicateM 1 $ normal center $ P2 (w / 4)
                                                                     (h / 4)

  rect    <- getBounds

  pathRef <- liftIO $ newIORef randomPath
  liftIO $ SDL.setMouseLocationMode SDL.AbsoluteLocation
  windowScale <- asks gcScale
  noise       <- newNoise2

  renderLoop $ do
    ps@(p NE.:| _) <- liftIO $ readIORef pathRef

    SDL.P mouseLoc <- liftIO SDL.getAbsoluteMouseLocation
    checkButtons   <- liftIO SDL.getMouseButtons

    let c = if checkButtons SDL.ButtonLeft
          then
            let targetLoc = fmap ((/ windowScale) . fromIntegral) mouseLoc
            in  lerp 0.05 p targetLoc
          else p

    mNext <- Just <$> normal
      c
      (fromIntegral (mouseLoc ^. _y) * pure (noise (c / 300)) * 0.001)

    case mNext of
      Nothing   -> pure ()
      Just next -> do
        let newPath = NE.fromList $ NE.take 400 $ next `NE.cons` ps
        liftIO $ writeIORef pathRef newPath

        fillScreenRGB white
        cairo $ do
          setSourceRGB black
          draw (PolygonOf newPath) *> fill

setup :: Generate ()
setup = do
  fillScreenRGB white
  cairo $ do
    setLineWidth 0.02
    setLineJoin LineJoinRound
    setLineCap LineCapRound
    setSourceRGB black
