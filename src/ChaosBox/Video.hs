module ChaosBox.Video
  ( renderFrame
  , eventLoop
  , registerEventHandler
  -- * Mouse events
  , onMouseDown
  , onMouseUp
  , onMouseMotion
  , syncHeldMousePosition
  -- * Keyboard events
  , onKeyDown
  , onKeyUp
  -- * Debugging
  , debugEvents
  -- * Re-exports
  , MouseButton(..)
  )
where

import           ChaosBox

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (unless, void, when)
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (asks)
import           Data.Foldable          (for_)
import           Data.IORef.Lifted
import qualified SDL
import           SDL.Event
import           System.CPUTime

-- | The main video rendering loop
--
-- This function causes the provided action to loop until the user expliclitly
-- exits ChaosBox. It will render one frame per loop according to the global
-- frame rate and handle each event according to any event handlers (registered
-- with 'registerEventHandler' or a variety of other functions in this module).
--
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

-- | Register a new event handler for an 'SDL.Event'
registerEventHandler :: (SDL.Event -> Generate ()) -> Generate ()
registerEventHandler handleEvent = do
  eventHandlerRef <- asks gcEventHandler
  modifyIORef eventHandlerRef $ \EventHandler {..} ->
    EventHandler $ \event -> ehHandleEvent event >> handleEvent event

-- | Print every 'SDL.Event' flowing through 'ChaosBox'
debugEvents :: Generate ()
debugEvents = registerEventHandler $ \event -> liftIO $ print event

-- | Do something when a 'MouseButton' is initially 'Pressed'
onMouseDown :: MouseButton -> (P2 -> Generate ()) -> Generate ()
onMouseDown button act = registerEventHandler $ \event ->
  case eventPayload event of
    MouseButtonEvent MouseButtonEventData {..} -> do
      windowScale <- asks gcScale
      when
          (mouseButtonEventMotion == Pressed && mouseButtonEventButton == button
          )
        $ do
            let SDL.P mouseLoc = mouseButtonEventPos
            act (fmap ((/ windowScale) . fromIntegral) mouseLoc)
    _ -> pure ()

-- | Do something when the specified 'MouseButton' is 'Released'
onMouseUp :: MouseButton -> (P2 -> Generate ()) -> Generate ()
onMouseUp button act = registerEventHandler $ \event ->
  case eventPayload event of
    MouseButtonEvent MouseButtonEventData {..} -> do
      windowScale <- asks gcScale
      when
          (  mouseButtonEventMotion
          == Released
          && mouseButtonEventButton
          == button
          )
        $ do
            let SDL.P mouseLoc = mouseButtonEventPos
            act (fmap ((/ windowScale) . fromIntegral) mouseLoc)
    _ -> pure ()

-- | Do something when the mouse moves
--
-- For example, this will print the location of the mouse every time it moves:
--
-- @onMouseMotion act (\p -> liftIO (print p))@
--
onMouseMotion :: (P2 -> Generate ()) -> Generate ()
onMouseMotion act = registerEventHandler $ \event -> case eventPayload event of
  MouseMotionEvent MouseMotionEventData {..} -> do
    windowScale <- asks gcScale
    let SDL.P mouseLoc = mouseMotionEventPos
    act (fmap ((/ windowScale) . fromIntegral) mouseLoc)
  _ -> pure ()

-- | Holds the current position of the Mouse while it is held down
--
-- When the specified 'MouseButton' is held down, this returns 'Just mousePos'
-- in user-space coordinates. Otherwise, it returns 'Nothing'.
syncHeldMousePosition :: MouseButton -> Generate (IORef (Maybe P2))
syncHeldMousePosition button = do
  clickedPointRef <- newIORef Nothing
  onMouseDown button $ writeIORef clickedPointRef . Just
  onMouseUp button $ \_ -> writeIORef clickedPointRef Nothing
  -- only update when mouse is down
  onMouseMotion $ \p -> do
    mPoint <- readIORef clickedPointRef
    for_ mPoint $ \_ -> writeIORef clickedPointRef (Just p)
  pure clickedPointRef

onKeyUp :: SDL.Scancode -> Generate () -> Generate ()
onKeyUp = undefined -- todo

-- | Render a frame one-off
--
-- By default, 'eventLoop' will render one frame at the end of each iteration
-- of the loop. If you want to render a frame one-off, you can use this
-- function to do so.
--
-- The rendered frame will be synced to the global frame rate.
--
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
