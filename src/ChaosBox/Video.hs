{-# LANGUAGE UndecidableInstances #-}
module ChaosBox.Video
  ( renderFrame
  , eventLoop
  , registerEventHandler
  -- * Mouse events
  , onMouseDown
  , onMouseUp
  , onMouseMotion
  , heldMousePosition
  -- * Keyboard events
  , onKeyDown
  , onKeyUp
  -- * 'Tick'
  , everyTick
  -- * Debugging
  , debugEvents
  -- * 'IORef' combinators
  , forRef
  , forRefM
  -- * Random utilities that should be moved
  , modifyIORefM
  -- * Re-exports
  , MouseButton(..)
  )
where

import           ChaosBox

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( unless
                                                , void
                                                , when
                                                )
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Foldable                  ( for_ )
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

    -- Handle a single 'Tick'
    ehHandleEvent Tick

    -- Handle all 'SDL.Event's
    events            <- liftIO SDL.pollEvents
    for_ events (ehHandleEvent . SDLEvent)
    unless (SDL.QuitEvent `elem` map SDL.eventPayload events) $ do
      void act
      renderFrame
      loop

-- | Register a new event handler for an 'SDL.Event'
registerEventHandler :: (ChaosBoxEvent -> Generate ()) -> Generate ()
registerEventHandler handleEvent = do
  eventHandlerRef <- asks gcEventHandler
  modifyIORef eventHandlerRef $ \EventHandler {..} ->
    EventHandler $ \event -> ehHandleEvent event >> handleEvent event

-- | Print every 'SDL.Event' flowing through 'ChaosBox'
debugEvents :: Generate ()
debugEvents = registerEventHandler $ \event -> liftIO $ print event

everyTick :: Generate () -> Generate ()
everyTick act = registerEventHandler  $ \case
  Tick -> act
  _ -> pure ()

-- | Do something when a 'MouseButton' is initially 'Pressed'
onMouseDown :: MouseButton -> (P2 -> Generate ()) -> Generate ()
onMouseDown button act = registerEventHandler . overSDLEvent $ \event ->
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
onMouseUp button act = registerEventHandler . overSDLEvent $ \event ->
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
onMouseMotion act = registerEventHandler . overSDLEvent $ \event ->
  case eventPayload event of
    MouseMotionEvent MouseMotionEventData {..} -> do
      windowScale <- asks gcScale
      let SDL.P mouseLoc = mouseMotionEventPos
      act (fmap ((/ windowScale) . fromIntegral) mouseLoc)
    _ -> pure ()

-- | Holds the current position of the Mouse while it is held down
--
-- When the specified 'MouseButton' is held down, this returns 'Just mousePos'
-- in user-space coordinates. Otherwise, it returns 'Nothing'.
--
heldMousePosition :: MouseButton -> Generate (IORef (Maybe P2))
heldMousePosition button = newSignal Nothing $ \ref -> do
  onMouseDown button $ writeIORef ref . Just
  onMouseUp button $ \_ -> writeIORef ref Nothing
  -- only update when mouse is down
  onMouseMotion $ \p -> do
    mPoint <- readIORef ref
    for_ mPoint $ \_ -> writeIORef ref (Just p)

newSignal :: a -> (IORef a -> Generate ()) -> Generate (IORef a)
newSignal def act = do
  ref <- newIORef def
  ref <$ act ref

onKeyUp :: SDL.Scancode -> Generate () -> Generate ()
onKeyUp = undefined -- todo

onKeyDown :: SDL.Scancode -> Generate () -> Generate ()
onKeyDown = undefined -- todo

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

modifyIORefM :: MonadBase IO m => IORef a -> (a -> m a) -> m ()
modifyIORefM ref f = do
  a <- readIORef ref
  b <- f a
  writeIORef ref b

forRef :: MonadBase IO m => IORef t -> (t -> b) -> m b
forRef b f = do
  b0 <- readIORef b
  pure (f b0)

forRefM :: MonadBase IO m => IORef t -> (t -> m b) -> m b
forRefM b f = do
  b0 <- readIORef b
  f b0
