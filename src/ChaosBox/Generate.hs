{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module ChaosBox.Generate where

import           Control.Arrow               ((&&&))
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.IORef
import qualified Data.Random                 as D
import           Data.Random.Internal.Source
import           Data.Semigroup
import           Graphics.Rendering.Cairo
import           Text.Printf

data GenerateCtx = GenerateCtx
  { gcWidth          :: Int
  , gcHeight         :: Int
  , gcSeed           :: Int
  , gcScale          :: Double
  , gcName           :: String
  , gcRenderProgress :: Bool
  , gcProgress       :: IORef Int
  , gcBeforeSaveHook :: IORef (Maybe (Generate ()))
  }

beforeSave :: Generate () -> Generate ()
beforeSave hook = do
  beforeSaveHookRef <- asks gcBeforeSaveHook
  liftIO $ writeIORef beforeSaveHookRef (Just hook)

type Generate a = RandT StdGen (ReaderT GenerateCtx Render) a

instance Monad m => D.MonadRandom (RandT StdGen (ReaderT GenerateCtx m)) where
    -- |Generate a uniformly distributed random 'Word8'
    getRandomWord8 = getRandom
    -- |Generate a uniformly distributed random 'Word16'
    getRandomWord16 = getRandom
    -- |Generate a uniformly distributed random 'Word32'
    getRandomWord32 = getRandom
    -- |Generate a uniformly distributed random 'Word64'
    getRandomWord64 = getRandom
    -- |Generate a uniformly distributed random 'Double' in the range 0 <= U < 1
    getRandomDouble = getRandom
    -- |Generate a uniformly distributed random 'Integer' in the range 0 <= U < 256^n
    getRandomNByteInteger n = getRandomR (0, 256^n)

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (gcWidth &&& gcHeight)
  pure (fromIntegral w, fromIntegral h)

renderProgress :: Generate ()
renderProgress = do
  doRender <- asks gcRenderProgress
  when doRender $ do
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

runGenerate :: Surface -> GenerateCtx -> Generate a -> IO (a, StdGen)
runGenerate surface ctx@GenerateCtx {..} doRender =
  renderWith surface
    . flip runReaderT ctx
    . flip runRandT   (mkStdGen gcSeed)
    $ do
        cairo $ scale gcScale gcScale
        doRender

-- | Lift a Cairo action into a Generate action
cairo :: Render a -> Generate a
cairo = lift . lift
