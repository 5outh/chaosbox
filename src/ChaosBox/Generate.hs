{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module ChaosBox.Generate where

import           ChaosBox.AABB
import           ChaosBox.Geometry.P2
import           Control.Arrow                 ((&&&))
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.IORef
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Random.Internal.Source
import           Data.Random.Source            as Source
import           GHC.Word                      (Word64)
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
  }

data VideoManager = VideoManager
  { vmFps                 :: Int
  -- ^ How many frames to render per second
  , vmLastRenderedTimeRef :: IORef Integer
  -- ^ The number of picoseconds since the last frame was rendered
  }

beforeSave :: Generate () -> Generate ()
beforeSave hook = do
  beforeSaveHookRef <- asks gcBeforeSaveHook
  liftIO $ writeIORef beforeSaveHookRef (Just hook)

type GenerateT m a = RandT PureMT (ReaderT GenerateCtx m) a
type Generate a = GenerateT Render a

$(monadRandom [d|
  instance Monad m => Source.MonadRandom (RandT PureMT (ReaderT GenerateCtx m)) where
    getRandomWord64 = liftRandT (pure . randomWord64)
  |])

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (gcWidth &&& gcHeight)
  pure (fromIntegral w, fromIntegral h)

getCenterPoint :: Generate P2
getCenterPoint = do
  (w, h) <- asks (gcWidth &&& gcHeight)
  pure $ V2 (fromIntegral w / 2) (fromIntegral h / 2)

renderProgress :: Generate ()
renderProgress = do
  let padInt :: Int -> String
      padInt = printf "%.8v"

  (name, progressRef) <- asks (gcName &&& gcProgress)
  progress            <- liftIO $ readIORef progressRef

  cairo . withTargetSurface $ \surface -> do
    liftIO . putStrLn $ "Rendering progress surface #" <> show progress
    liftIO
      $  surfaceWriteToPNG surface
      $  "images/"
      <> name
      <> "/progress/"
      <> padInt progress
      <> ".png"

  liftIO $ modifyIORef progressRef (+ 1)

runGenerate :: Surface -> GenerateCtx -> Generate a -> IO (a, PureMT)
runGenerate surface ctx@GenerateCtx {..} doRender =
  renderWith surface . flip runReaderT ctx . flip runRandT (pureMT gcSeed) $ do
    cairo $ scale gcScale gcScale
    doRender

-- | Lift a 'Render' (cairo) action into a 'Generate' action
cairo :: Render a -> Generate a
cairo = lift . lift

-- | Get the bounding 'AABB' for the image surface
getBounds :: Generate AABB
getBounds = do
  (w, h) <- getSize
  pure $ boundary $ 0 :| [P2 w h]
