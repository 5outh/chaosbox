{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module ChaosBox.Generate
  (
  -- * Generate's 'Reader' context
    GenerateCtx(..)
  , VideoManager(..)
  -- * 'ChaosBoxEvent' management
  , EventHandler(..)
  , ChaosBoxEvent(..)
  , overSDLEvent
  -- * Hooks
  , beforeSave
  -- * Effects
  , Generate
  , GenerateT
  -- * Context-sensitive rendering utilities
  , renderProgress
  , getSize
  , getCenterPoint
  , getBounds
  -- * Useful aliases
  , cairo
  )
where

import           ChaosBox.Orphanage             ( )
import           ChaosBox.AABB
import           ChaosBox.Geometry.P2

import           Control.Monad.Base
import           System.Directory
import           Control.Arrow                  ( (&&&) )
import           Control.Monad.Random
import           Control.Monad.Reader
import           UnliftIO.IORef
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Random.Internal.Source
import           Data.Random.Source            as Source
import           GHC.Word                       ( Word64 )
import           GI.Cairo.Render
import           Linear.V2
import qualified SDL
import           System.Random.Mersenne.Pure64
import           Text.Printf

data GenerateCtx = GenerateCtx
  { gcWidth          :: Int
  -- ^ Width of the canvas
  , gcHeight         :: Int
  -- ^ Height of the canvas
  , gcSeed           :: Word64
  -- ^ Seed for random generation
  , gcScale          :: Double
  -- ^ Scaling factor for 'gcWidth' and 'gcHeight' to generate the final pixel
  -- size of the output
  , gcName           :: String
  -- ^ Name of the current project
  , gcProgress       :: IORef Int
  -- ^ Current progress "tick"
  , gcBeforeSaveHook :: IORef (Maybe (Generate ()))
  -- ^ Action to perform before saving the image.
  , gcCairoSurface   :: Surface
  -- ^ Raw mutable cairo Surface
  , gcWindow         :: Maybe SDL.Window
  -- ^ SDL 'Window' to display image in
  , gcVideoManager   :: VideoManager
  -- ^ Video manager
  , gcEventHandler   :: IORef EventHandler
  -- ^ Mutable Event Handler
  , gcMetadataString :: Maybe String
  -- ^ Optional string to append to file name
  }

data ChaosBoxEvent
  = Tick
  -- ^ A single loop of 'ChaosBox.Video.eventLoop' has passed
  | SDLEvent SDL.Event
  -- ^ An 'SDL.Event' has occurred
  deriving (Show, Eq)

-- | Build an event handler for an 'SDL.Event'
overSDLEvent :: Applicative f => (SDL.Event -> f ()) -> ChaosBoxEvent -> f ()
overSDLEvent f = \case
  Tick           -> pure ()
  SDLEvent event -> f event

-- TODO: Implement this interface
--
-- newtype EventHandler = EventHandler { ehEventHandlers :: IntMap (ChaosBoxEvent -> Generate ())}
-- newtype EventHandlerId = EventHandlerId !Int
-- registerEventHandler :: (ChaosboxEvent -> Generate ()) -> Generate EventHandlerId
-- unregisterEventHandler :: EventHandlerId -> Generate ()

-- | How to handle 'ChaosBoxEvent's
--
-- New event handlers can be registered with
-- 'ChaosBox.Interactive.registerEventHandler' and other functions in that
-- module.
--
newtype EventHandler = EventHandler { ehHandleEvent :: ChaosBoxEvent -> Generate () }

-- | Interactive video data
data VideoManager = VideoManager
  { vmFps                 :: Int
  -- ^ How many frames to render per second
  , vmLastRenderedTimeRef :: IORef Integer
  -- ^ The number of picoseconds since the last frame was rendered
  }

-- | Register an action to be performed before an image is saved
beforeSave :: Generate () -> Generate ()
beforeSave hook = do
  beforeSaveHookRef <- asks gcBeforeSaveHook
  writeIORef beforeSaveHookRef (Just hook)

type GenerateT m a = RandT PureMT (ReaderT GenerateCtx m) a
instance MonadBase Render m => MonadBase Render (RandT PureMT (ReaderT GenerateCtx m)) where
  liftBase = liftBaseDefault

-- | The main 'ChaosBox' Monad
--
-- Supports the following effects:
--
-- - Writing to a cairo canvas via 'Render'
-- - Reading the 'GenerateCtx'
-- - Randomly generating values via 'RandT' 'PureMT', which hooks nicely into
-- @MonadRandom@ and @random-fu@.
--
type Generate a = GenerateT Render a

$(monadRandom [d|
  instance Monad m => Source.MonadRandom (RandT PureMT (ReaderT GenerateCtx m)) where
    getRandomWord64 = liftRandT (pure . randomWord64)
  |])

-- | Get the @(width, height)@ pair of the current surface in user-space
getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (gcWidth &&& gcHeight)
  pure (fromIntegral w, fromIntegral h)

-- | Get the center 'P2' of the current surface in user-space
getCenterPoint :: Generate P2
getCenterPoint = do
  (w, h) <- asks (gcWidth &&& gcHeight)
  pure $ V2 (fromIntegral w / 2) (fromIntegral h / 2)

-- | Render an "in-progress" image to @./images/$name/$seed/$progress/$N@
--
--  @N@ is the number of times 'renderProgress' has been previously called.
--  For example:
--
--  @
--  center <- getCenterPoint
--  circleRef <- newIORef $ Circle center 1
--
--  replicateM_ 100 $ do
--    fillScreenRGB white
--    cairo $ setSourceRGB black
--    modifyIORef circleRef $ \c -> c { circleRadius = circleRadius c + 1 }
--    circle  <- readIORef circleRef
--    cairo $
--      draw circle *> stroke
--    renderProgress
--  @
--
--  This will render a sequence of images that show a circle growing from the
--  center of the image from 2 units in diameter to 50 units in diameter.
--
--  This function is useful for writing @ffmpeg@ scripts to render videos from
--  @ChaosBox@ output -- the sequence generated in the @progress@ folder is
--  suitable for @ffmpeg@.
--
renderProgress :: Generate ()
renderProgress = do
  (name, progressRef) <- asks (gcName &&& gcProgress)
  liftIO $ createDirectoryIfMissing False $ "./images/" <> name <> "/progress"
  let padInt :: Int -> String
      padInt = printf "%.8v"

  progress <- readIORef progressRef

  cairo . withTargetSurface $ \surface -> do
    liftIO . putStrLn $ "Rendering progress surface #" <> show progress
    liftIO
      $  surfaceWriteToPNG surface
      $  "images/"
      <> name
      <> "/progress/"
      <> padInt progress
      <> ".png"

  modifyIORef progressRef (+ 1)

-- | Lift a 'Render' (cairo) action into a 'GenerateT' action
cairo :: MonadBase Render m => Render a -> GenerateT m a
cairo = lift . lift . liftBase

-- | Get the bounding 'ChaosBox.Geometry.AABB' for the screen or image
getBounds :: Generate AABB
getBounds = do
  (w, h) <- getSize
  pure $ boundary $ 0 :| [P2 w h]
