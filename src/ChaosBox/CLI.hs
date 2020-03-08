{-# LANGUAGE OverloadedStrings #-}
-- | Utilities for creating CLI applications that interface with 'ChaosBox'
module ChaosBox.CLI
  ( runChaosBoxWith
  , runChaosBoxIO
  , runChaosBoxIOWith
  , runChaosBoxInteractive
  , getDefaultOpts
  , Opts(..)
  )
where

import           ChaosBox.Generate
import           Control.Concurrent
import           Control.Monad                 (unless)
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.IORef
import           Data.Maybe                    (fromMaybe)
import           Data.Semigroup                ((<>))
import           Data.Time.Clock.POSIX
import           GHC.Word
import           GI.Cairo.Render
import           Options.Applicative
import           System.Directory
import           System.Random.Mersenne.Pure64

import           Foreign.Ptr                   (castPtr)
import           SDL

data Opts = Opts
  { optSeed           :: Maybe Word64
  -- ^ Random seed used for all PRNG.
  , optScale          :: Double
  -- ^ Scale applied to user-space to produce final image
  , optWidth          :: Int
  -- ^ Width in user-space
  , optHeight         :: Int
  -- ^ Height in user-space
  , optRenderTimes    :: Int
  -- ^ How many times to repeat rendering, helpful for fast experimentation
  , optName           :: String
  -- ^ Name of the process. Images will be stored at
  -- @images/${optName}-${optSeed}.png@
  , optRenderProgress :: Bool
  -- Enable in-progress rendering via 'renderProgress'
  , optMetadataString :: Maybe String
  -- Optional string to append to file name, useful for tagging
  }

getDefaultOpts :: IO Opts
getDefaultOpts = do
  seed <- round . (* 1000) <$> getPOSIXTime
  pure Opts { optSeed           = Just seed
            , optScale          = 1
            , optWidth          = 100
            , optHeight         = 100
            , optRenderTimes    = 1
            , optName           = "sketch"
            , optRenderProgress = True
            , optMetadataString = Nothing
            }

opts :: Parser Opts
opts =
  Opts
    <$> optional (option auto $ long "seed" <> metavar "SEED")
    <*> option auto (long "scale" <> metavar "SCALE" <> value 1)
    <*> option auto (long "width" <> short 'w' <> metavar "WIDTH" <> value 100)
    <*> option auto
               (long "height" <> short 'h' <> metavar "HEIGHT" <> value 100)
    <*> option auto (long "times" <> metavar "TIMES" <> value 1)
    <*> strOption (long "name" <> metavar "NAME" <> value "sketch")
    <*> switch (long "render-progress" <> short 'r')
    <*> optional (strOption (long "metadata" <> metavar "METADATA"))

optsInfo :: ParserInfo Opts
optsInfo = info
  (opts <**> helper)
  (fullDesc <> progDesc "Generate art with ChaosBox" <> header "chaosbox")

-- | Run 'ChaosBox' with 'Opts' parsed from the CLI
runChaosBoxIO
  :: RandT PureMT (ReaderT GenerateCtx Render) a
  -- ^ Render function
  -> IO ()
runChaosBoxIO = runChaosBoxIOWith id

-- | Run 'ChaosBox' with 'Opts' parsed from the CLI, allowing overrides.
runChaosBoxIOWith
  :: (Opts -> Opts)
  -- ^ Option modifier
  -> RandT PureMT (ReaderT GenerateCtx Render) a
  -- ^ Render function
  -> IO ()
runChaosBoxIOWith fn render = do
  options <- execParser optsInfo
  runChaosBoxWith (fn options) render

-- | Run 'ChaosBox' with the given 'Opts'
runChaosBoxWith
  :: Opts
  -- ^ Art options
  -> RandT PureMT (ReaderT GenerateCtx Render) a
  -- ^ Render function
  -> IO ()
runChaosBoxWith Opts {..} doRender = replicateM_ optRenderTimes $ do
  seed <- case optSeed of
    Just seed' -> pure seed'
    Nothing    -> round . (* 1000) <$> getPOSIXTime

  let stdGen = pureMT seed
      w      = round $ fromIntegral optWidth * optScale
      h      = round $ fromIntegral optHeight * optScale

  surface     <- createImageSurface FormatARGB32 w h
  progressRef <- newIORef 0

  -- Create directories if they don't exist
  createDirectoryIfMissing False "./images/"
  createDirectoryIfMissing False $ "./images/" <> optName
  createDirectoryIfMissing False $ "./images/" <> optName <> "/progress"

  beforeSaveHookRef <- newIORef Nothing

  let ctx = GenerateCtx optWidth
                        optHeight
                        seed
                        optScale
                        optName
                        optRenderProgress
                        progressRef
                        beforeSaveHookRef
                        surface
                        Nothing

  void . renderWith surface . flip runReaderT ctx . flip runRandT stdGen $ do
    cairo $ scale optScale optScale
    void doRender

    ref   <- asks gcBeforeSaveHook
    mHook <- liftIO $ readIORef ref

    fromMaybe (pure ()) mHook

  putStrLn "Generating art..."
  let filename =
        "images/"
          <> optName
          <> "/"
          <> show seed
          <> "-"
          <> show optScale
          <> fromMaybe "" optMetadataString
          <> ".png"
      latest = "images/" <> optName <> "/latest.png"

  putStrLn $ "Writing " <> filename
  putStrLn $ "Writing " <> latest

  surfaceWriteToPNG surface filename
  surfaceWriteToPNG surface latest

-- | Run 'ChaosBox' with the given 'Opts'
runChaosBoxInteractive
  :: Opts
  -- ^ Art options
  -> RandT PureMT (ReaderT GenerateCtx Render) ()
  -- ^ Render function
  -> IO ()
runChaosBoxInteractive Opts {..} doRender = replicateM_ optRenderTimes $ do
  seed <- case optSeed of
    Just seed' -> pure seed'
    Nothing    -> round . (* 1000) <$> getPOSIXTime

  let stdGen       = pureMT seed
      screenWidth  = round $ fromIntegral optWidth * optScale
      screenHeight = round $ fromIntegral optHeight * optScale

      -- screenWidth = 600
      -- screenHeight = 600


  -- surface     <- createImageSurface FormatARGB32 screenWidth screenHeight
  progressRef <- newIORef 0

  -- Create directories if they don't exist
  createDirectoryIfMissing False "./images/"
  createDirectoryIfMissing False $ "./images/" <> optName
  createDirectoryIfMissing False $ "./images/" <> optName <> "/progress"

  beforeSaveHookRef <- newIORef Nothing

  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow
    "SDL / Cairo Example"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window
  let white = V4 maxBound maxBound maxBound maxBound
  SDL.surfaceFillRect screenSurface Nothing white
  pixels <- castPtr <$> surfacePixels screenSurface

  canvas <- createImageSurfaceForData pixels
                                      FormatRGB24
                                      (fromIntegral screenWidth)
                                      (fromIntegral screenHeight)
                                      (fromIntegral $ screenWidth * 4)

  let ctx = GenerateCtx optWidth
                        optHeight
                        seed
                        optScale
                        optName
                        optRenderProgress
                        progressRef
                        beforeSaveHookRef
                        canvas
                        (Just window)
  -- renderWith canvas demo1

  -- withImageSurfaceForData pixels FormatRGB24 600 600 (600 * 4)
    -- $ \canvas -> renderWith canvas demo2

  void . renderWith canvas . flip runReaderT ctx . flip runRandT stdGen $ do
    cairo $ scale optScale optScale
    void doRender

        -- ref <- asks gcBeforeSaveHook
        -- mHook <- liftIO $ readIORef ref

        -- fromMaybe (pure ()) mHook

  SDL.updateWindowSurface window

  putStrLn "Generating art..."

  let filename =
        "images/"
          <> optName
          <> "/"
          <> show seed
          <> "-"
          <> show optScale
          <> fromMaybe "" optMetadataString
          <> ".png"
      latest = "images/" <> optName <> "/latest.png"

  putStrLn $ "Writing " <> filename
  putStrLn $ "Writing " <> latest

  surfaceWriteToPNG canvas filename
  surfaceWriteToPNG canvas latest
  forkIO idle
 where
  idle = do
    events <- liftIO SDL.pollEvents
    let shouldQuit = elem SDL.QuitEvent $ map SDL.eventPayload events
    unless shouldQuit idle
