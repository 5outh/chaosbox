module ChaosBox.Video
  ( renderFrame
  , renderLoop
  )
where

import           ChaosBox

import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class
import           Control.Monad.Loops    (untilM_)
import           Control.Monad.Reader   (asks)
import           Data.Foldable          (for_)
import           Data.IORef
import qualified SDL
import           System.CPUTime

-- |
renderLoop :: MonadIO m => GenerateT m a -> GenerateT m ()
renderLoop act = (act >> renderFrame) `untilM_` shouldQuitM
 where
  shouldQuitM = do
    events <- liftIO SDL.pollEvents
    pure $ elem SDL.QuitEvent $ map SDL.eventPayload events

renderFrame :: MonadIO m => GenerateT m ()
renderFrame = do
  mWindow <- asks gcWindow

  for_ mWindow $ \window -> do
    VideoManager {..} <- asks gcVideoManager
    liftIO $ do
      now                   <- getCPUTime
      lastFrameRenderedTime <- readIORef vmLastRenderedTimeRef

      let targetSeconds :: Double
          targetSeconds = 1 / fromIntegral vmFps
          lastFrameRenderedTimeSeconds =
            fromIntegral lastFrameRenderedTime * 10 ** (-12)
          targetTimeSeconds = lastFrameRenderedTimeSeconds + targetSeconds
          waitDiffSeconds   = targetTimeSeconds - lastFrameRenderedTimeSeconds
          waitNs            = max 0 $ floor (waitDiffSeconds * 1000000)

      threadDelay waitNs
      SDL.updateWindowSurface window
      writeIORef vmLastRenderedTimeRef now
