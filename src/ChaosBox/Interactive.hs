{-# LANGUAGE UndecidableInstances #-}
-- | The Interactive Guts of ChaosBox
--
-- This module contains the bits and pieces needed to wire up an interactive
-- ChaosBox program. Internally, ChaosBox renders a @cairo@ 'Surface' to an
-- 'SDL.window'. To make a program interactive, 'ChaosBox' requires the user to
-- write an 'eventLoop'. This 'eventLoop' has two key functions:
--
-- 1. It renders a single frame at the user-provided fps
-- 2. It processes all new 'ChaosBoxEvent's each frame
--
-- ChaosBox heavily leverages 'SDL', so most 'ChaosBoxEvent's are just
-- 'SDL.Event's (See 'SDLEvent'). This module provides some basic
-- functionality for registering event handlers for various user actions, like
-- keyboard and mouse input.
--
-- Because an event loop is naturally stateful, 'IORef's are a key player in
-- interactive ChaosBox programs. A typical interactive ChaosBox program
-- might look something like this:
--
-- @
-- center <- 'getCenterPoint'
-- let centerCircle = 'Circle' center 0
-- circleRef <- 'newIORef' centerCircle
--
-- 'onMouseDown' $ \p -> do
--  'modifyIORefM_' circleRef $ \c -> do
--    newCenter <- 'normal' p 1
--    let newRadius = 'circleRadius' c + 0.1
--    pure $ 'Circle' newCenter newRadius
--
-- 'cairo' $ 'setSourceRGB' 'black'
-- 'eventLoop' $ do
--   c <- 'readIORef' circleRef
--   'cairo' $ do
--     'draw' c *> 'stroke'
-- @
--
-- This will draw a 'Circle' each time the mouse is clicked. The center of that
-- 'Circle' will be nearby where the user clicks, and each new circle drawn
-- will have a radius @0.1@ units larger than the previous circle drawn.
--
-- See 'eventLoop' for more information, including a list of default event
-- handlers / key bindings.
--
module ChaosBox.Interactive
  (
  -- * Dealing with events
    eventLoop
  , eventLoopN
  , registerEventHandler
  -- * kOne-off rendering
  , renderFrame
  -- * Mouse events
  , onMouseDown
  , onMouseUp
  , onMouseMotion
  , heldMousePosition
  -- * Keyboard events
  , onKeyDown
  , bindKey
  , onKeyUp
  , syncKeyDown
  , syncKeyUp
  , whileKeyDown
  , whileKeyUp
  -- * 'Tick'
  , everyTick
  -- * Debugging
  , debugEvents
  -- * 'IORef' combinators
  , readIORefWith
  , forIORef
  , readIORefWithM
  , forIORefM
  , modifyIORefM
  , modifyIORefM_
  -- * Re-exports
  , MouseButton(..)
  )
where

import           ChaosBox.CLI                   ( saveImageWith )
import           ChaosBox.Generate
import           ChaosBox.Geometry.P2
import           ChaosBox.Random                ( unsafeUniform )

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( unless
                                                , void
                                                , when
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Foldable                  ( for_ )
import qualified SDL
import           SDL.Event
import           System.CPUTime
import           UnliftIO.IORef

-- | The main video rendering loop
--
-- This function causes the provided action to loop until the user expliclitly
-- exits ChaosBox. It will render one frame per loop according to the global
-- frame rate and handle each event according to any event handlers (registered
-- with 'registerEventHandler' or a variety of other functions in this module).
--
-- Beyond handling any user-specified events, there are a couple of default
-- behaviors registered here:
--
--  - One 'Tick' is processed per loop.
--  - Pressing @s@ saves the current image on-screen.
--  - Pressing @q@ or clicking the @x@ button will immediately quit 'ChaosBox'.
--
eventLoop :: Generate a -> Generate ()
eventLoop act = do
  bindKey SDL.ScancodeS $ do
    str <- replicateM 6 $ unsafeUniform ['a' .. 'z']
    saveImageWith (Just str)
  -- TODO: This isn't perfect, could bind the key down event directly to the
  -- "quit" action
  shouldQuitRef <- syncKeyDown SDL.ScancodeQ
  loop shouldQuitRef
 where
  loop shouldQuitRef = do
    EventHandler {..} <- readIORef =<< asks gcEventHandler

    -- Handle a single 'Tick'
    ehHandleEvent Tick

    -- Handle all 'SDL.Event's
    events <- liftIO SDL.pollEvents
    for_ events (ehHandleEvent . SDLEvent)
    shouldQuit <- readIORef shouldQuitRef
    unless (SDL.QuitEvent `elem` map SDL.eventPayload events || shouldQuit) $ do
      void act
      renderFrame
      loop shouldQuitRef

-- | Same as 'eventLoop', but only run for some specified number of frames.
eventLoopN :: Int -> Generate a -> Generate ()
eventLoopN n act = do
  bindKey SDL.ScancodeS $ do
    str <- replicateM 6 $ unsafeUniform ['a' .. 'z']
    saveImageWith (Just str)
  shouldQuitRef <- syncKeyDown SDL.ScancodeQ

  loop n shouldQuitRef
 where
  loop 0 _             = pure ()
  loop m shouldQuitRef = do
    EventHandler {..} <- readIORef =<< asks gcEventHandler

    -- Handle a single 'Tick'
    ehHandleEvent Tick

    -- Handle all 'SDL.Event's
    events <- liftIO SDL.pollEvents
    for_ events (ehHandleEvent . SDLEvent)
    shouldQuit <- readIORef shouldQuitRef
    unless (SDL.QuitEvent `elem` map SDL.eventPayload events || shouldQuit) $ do
      void act
      renderFrame
      loop (pred m) shouldQuitRef

-- | Register a new event handler for an 'SDL.Event'
registerEventHandler :: (ChaosBoxEvent -> Generate ()) -> Generate ()
registerEventHandler handleEvent = do
  eventHandlerRef <- asks gcEventHandler
  modifyIORef eventHandlerRef $ \EventHandler {..} ->
    EventHandler $ \event -> ehHandleEvent event >> handleEvent event

-- | Print every 'SDL.Event' flowing through 'ChaosBox'
debugEvents :: Generate ()
debugEvents = registerEventHandler $ \event -> liftIO $ print event

-- | Perform some action once per 'Tick'
everyTick :: Generate () -> Generate ()
everyTick act = registerEventHandler $ \case
  Tick -> act
  _    -> pure ()

-- | Do something when the specified 'MouseButton' is 'Pressed'
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

-- | Do something with the mouse's user-space position when the mouse moves
--
-- For example, this registers an event handler that prints the mouse's
-- position every time it moves:
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

-- | Do something when a key is 'Released'
onKeyUp :: SDL.Scancode -> Generate () -> Generate ()
onKeyUp scancode act = registerEventHandler . overSDLEvent $ \event ->
  case eventPayload event of
    KeyboardEvent KeyboardEventData {..}
      | (  SDL.keysymScancode keyboardEventKeysym
        == scancode
        && keyboardEventKeyMotion
        == Released
        )
      -> act
    _ -> pure ()

-- | Do something when a key is 'Pressed'
onKeyDown :: SDL.Scancode -> Generate () -> Generate ()
onKeyDown scancode act = registerEventHandler . overSDLEvent $ \event ->
  case eventPayload event of
    KeyboardEvent KeyboardEventData {..}
      | (  SDL.keysymScancode keyboardEventKeysym
        == scancode
        && keyboardEventKeyMotion
        == Pressed
        )
      -> act
    _ -> pure ()

-- | Alias for 'onKeyDown'
bindKey :: SDL.Scancode -> Generate () -> Generate ()
bindKey = onKeyDown

-- | Return a an 'IORef' containing whether a key is currently 'Pressed'
syncKeyDown :: SDL.Scancode -> Generate (IORef Bool)
syncKeyDown scancode = newSignal False $ \ref -> do
  onKeyDown scancode $ writeIORef ref True
  onKeyUp scancode $ writeIORef ref False

-- | Return a an 'IORef' containing whether a key is currently 'Released'
syncKeyUp :: SDL.Scancode -> Generate (IORef Bool)
syncKeyUp scancode = newSignal True $ \ref -> do
  onKeyDown scancode $ writeIORef ref False
  onKeyUp scancode $ writeIORef ref True

-- | Do something each 'Tick' while a key is 'Pressed'
whileKeyDown :: SDL.Scancode -> Generate () -> Generate ()
whileKeyDown scancode act = do
  isKeyDown <- syncKeyDown scancode
  everyTick $ do
    isDown <- readIORef isKeyDown
    when isDown act

-- | Do something each 'Tick' while a key is 'Released'
whileKeyUp :: SDL.Scancode -> Generate () -> Generate ()
whileKeyUp scancode act = do
  isKeyDown <- syncKeyUp scancode
  everyTick $ do
    isDown <- readIORef isKeyDown
    when isDown act

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

-- | Monadic 'modifyIORef' which returns the value written.
modifyIORefM :: MonadIO m => IORef a -> (a -> m a) -> m a
modifyIORefM ref f = do
  a <- readIORef ref
  b <- f a
  b <$ writeIORef ref b

-- | Monadic 'modifyIORef'
modifyIORefM_ :: MonadIO m => IORef a -> (a -> m a) -> m ()
modifyIORefM_ ref = void . modifyIORefM ref

-- | 'readIORef', running a function on the output
readIORefWith :: MonadIO m => (t -> b) -> IORef t -> m b
readIORefWith f b = do
  b0 <- readIORef b
  pure (f b0)

-- | Flipped 'readIORef', sometimes reads better
forIORef :: MonadIO m => IORef t -> (t -> b) -> m b
forIORef = flip readIORefWith

-- | Monadic 'readIORefWith'
readIORefWithM :: MonadIO m => (t -> m b) -> IORef t -> m b
readIORefWithM f b = do
  b0 <- readIORef b
  f b0

-- | Flipped 'readIORefWithM', sometimes reads better
forIORefM :: MonadIO m => IORef t -> (t -> m b) -> m b
forIORefM = flip readIORefWithM

-- Utilities

newSignal :: a -> (IORef a -> Generate ()) -> Generate (IORef a)
newSignal def act = do
  ref <- newIORef def
  ref <$ act ref
