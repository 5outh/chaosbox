module ChaosBox.Video
  ( renderFrame
  , renderLoop
  , eventLoop
  , registerEventHandler
  , onClick
  )
where

import           ChaosBox

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (unless, void, when)
import           Control.Monad.IO.Class
import           Control.Monad.Loops    (untilM_)
import           Control.Monad.Reader   (asks)
import           Data.Foldable          (for_)
import           Data.IORef.Lifted
import qualified SDL
import           SDL.Event
import           System.CPUTime

-- |
renderLoop :: MonadIO m => GenerateT m a -> GenerateT m ()
renderLoop act = (act >> renderFrame) `untilM_` shouldQuitM
 where
  shouldQuitM = do
    events <- liftIO SDL.pollEvents
    pure $ elem SDL.QuitEvent $ map SDL.eventPayload events

eventLoop :: Generate a -> Generate ()
eventLoop act = loop
 where
  loop = do
    EventHandler {..} <- readIORef =<< asks gcEventHandler
    events            <- liftIO SDL.pollEvents
    -- liftIO $ print events
    for_ events ehHandleEvent
    unless (SDL.QuitEvent `elem` map SDL.eventPayload events) $ do
      void act
      renderFrame
      loop

registerEventHandler :: (SDL.Event -> Generate ()) -> Generate ()
registerEventHandler handleEvent = do
  eventHandlerRef <- asks gcEventHandler
  modifyIORef eventHandlerRef $ \EventHandler {..} ->
    EventHandler $ \event -> ehHandleEvent event >> handleEvent event

-- | Print every 'SDL.Event' flowing through 'ChaosBox'
debugEvents :: Generate ()
debugEvents = registerEventHandler $ \event -> liftIO $ print event

onClick :: (P2 -> Generate ()) -> Generate ()
onClick act = registerEventHandler $ \event -> case eventPayload event of
  MouseButtonEvent MouseButtonEventData {..} -> do
    windowScale <- asks gcScale
    when
        (  mouseButtonEventMotion
        == Pressed
        && mouseButtonEventButton
        == ButtonLeft
        )
      $ do
          let SDL.P mouseLoc = mouseButtonEventPos
          act mouseLoc
  _ -> pure ()

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
