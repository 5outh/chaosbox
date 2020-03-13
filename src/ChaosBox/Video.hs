{-# LANGUAGE UndecidableInstances #-}
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
  -- * Behavior combinators
  , withBehavior
  , withBehaviorM
  -- * Re-exports
  , MouseButton(..)
  -- * Behaviors
  , Behavior(..)
  , behavior
  , grab
  )
where

import           ChaosBox

import           Control.Concurrent            (threadDelay)
import           Control.Monad                 (unless, void, when)
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Foldable                 (for_)
import           Data.IORef.Lifted
import qualified SDL
import           SDL.Event
import           System.CPUTime
import           System.Random.Mersenne.Pure64

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

-- is *this* just a behavior???
-- | Register a new event handler for an 'SDL.Event'
registerEventHandler :: (SDL.Event -> Generate ()) -> Generate ()
registerEventHandler handleEvent = do
  eventHandlerRef <- asks gcEventHandler
  modifyIORef eventHandlerRef $ \EventHandler {..} ->
    EventHandler $ \event -> ehHandleEvent event >> handleEvent event

-- | Print every 'SDL.Event' flowing through 'ChaosBox'
debugEvents :: Generate ()
debugEvents = registerEventHandler $ \event -> liftIO $ print event

-- Note this doesn't actually *register* event handlers, it just encompasses a
-- value whose value is tracked over time
-- rawEvent
  -- :: MonadBase IO m
  -- => SDL.Event
  -- -> Behavior (RandT PureMT (ReaderT GenerateCtx m)) SDL.Event
  -- m ((a -> m a) -> m a)
-- rawEvent = pure

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
--
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

-- TODO: Want some notion of "a variable that updates on given events"
-- e.g. on tick => update an IORef
--
-- 'syncHeldMousePosition' is an example of this

newtype Behavior m a = Behavior { runBehavior :: m ((a -> m a) -> m a) }

-- I think the key here might be the Monad instance of Behavior (`bind`)
-- clickedPoint <- behavior Nothing
-- downPoint <- clickedPoint $ \mp -> case mp of
--   Nothing -> pure Nothing
--   Just a -> pure (Just a)
--  upPoint <- downPoint $ \mp -> Nothing
--

-- (a -> m a) is an _event handler_, it describes how something changes

getBehavior :: MonadBase IO m => Behavior m a -> m a
getBehavior (Behavior ba) = do
  fa <- ba
  fa pure

grab :: MonadBase IO m => Behavior m a -> m a
grab = getBehavior

behavior :: MonadBase IO m => a -> Behavior m a
behavior a = Behavior $ do
  a0 <- newIORef a
  pure $ \f -> do
    a1   <- readIORef a0
    next <- f a1
    next <$ writeIORef a0 next

-- how do these compose, that is the question

-- smash
  -- :: MonadBase IO m
  -- => (a -> b -> c)
  -- -> Behavior m a -- Behavior a
  -- -> Behavior m b -- Behavior b
  -- -> Behavior m c -- Behavior c
-- smash f ba bb = do
  -- fa <- ba
  -- fb <- bb
  -- a  <- fa pure
  -- b  <- fb pure
  -- behavior (f a b)

mapBehavior :: MonadBase IO m => (a -> b) -> Behavior m a -> Behavior m b
mapBehavior f ba = Behavior $ do
  a <- getBehavior ba
  runBehavior (behavior (f a))

instance MonadBase IO m => Functor (Behavior m) where
  fmap = mapBehavior

appBehavior
  :: MonadBase IO m => Behavior m (a -> b) -> Behavior m a -> Behavior m b
appBehavior fb ba = Behavior $ do
  f <- getBehavior fb
  a <- getBehavior ba
  runBehavior (behavior (f a))

instance MonadBase IO m => Applicative (Behavior m) where
  pure  = behavior
  (<*>) = appBehavior

bindBehavior
  :: MonadBase IO m => Behavior m a -> (a -> Behavior m b) -> Behavior m b
bindBehavior ba f = Behavior $ do
  a <- getBehavior ba
  runBehavior (f a)

instance MonadBase IO m => Monad (Behavior m) where
  (>>=) = bindBehavior

-- foo :: MonadBase IO m => Behavior m Int
-- foo = do
  -- n <- behavior 0
  -- m <- behavior "100"
  -- pure $ n + read m

-- this is a composed behavior
-- foo :: IO ((Int -> IO Int) -> IO Int)
-- foo = do
  -- n <- behavior 0
  -- m <- behavior "100"
  -- smash (\a b -> a + read b) (behavior 0) (behavior "100")

-- compose :: Behavior a -> Behavior a -> Behavior a
-- compose fa fb = do
  -- n <- fa
  -- m <- fb
  -- pure $ \a -> do
   -- b :: a <- n a
   -- c :: a <- m b
   -- m _c
--
-- n <- behavior 0
-- m <- behavior 2
-- -- would apply to BOTH n and m, then return the result
-- t <- compose n m
-- b <- (0 + 1) -- updates 'n'
-- m (+1)       -- updates 'm'
--
-- -- result: both 'n' and 'm' get incremented by 1, t = n + m and is reactive
--
-- combine :: (a -> b -> c) -> Behavior a a -> Behavior d b -> Behavior a c
--
-- n <- behavior 0
-- m <- behavior "100"
-- res <- combine (\n0 m0 -> n0 + read m0) n m
--
-- res = 100
--
-- n (+1)
--
-- res = 101
--


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

withBehavior :: MonadBase IO m => IORef t -> (t -> b) -> m b
withBehavior b f = do
  b0 <- readIORef b
  pure (f b0)

withBehaviorM :: MonadBase IO m => IORef t -> (t -> m b) -> m b
withBehaviorM b f = do
  b0 <- readIORef b
  f b0
