{-# LANGUAGE OverloadedStrings #-}
-- | Utilities for creating CLI applications that interface with @ChaosBox@
module ChaosBox.CLI
  (
  -- * ChaosBox options
    Opts(..)
  , getDefaultOpts
  -- * Running ChaosBox programs
  , runChaosBox
  , runChaosBoxWith
  , runChaosBoxDirectly
  -- * Saving images
  , saveImage
  , saveImageWith
  )
where


import           ChaosBox.Generate

import           ChaosBox.Orphanage()
import           Data.Foldable                  ( for_ )
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.IORef.Lifted
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Time.Clock.POSIX
import           GHC.Word
import           GI.Cairo.Render
import           Options.Applicative
import           System.Directory
import           System.Random.Mersenne.Pure64

import           Foreign.Ptr                    ( castPtr )
import           SDL

data RenderMode = Static | Interactive

-- | ChaosBox's options
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
  -- @images/${optName}.${optName}-${optSeed}.png@
  , optMetadataString :: Maybe String
  -- ^ Optional string to append to file name, useful for tagging
  , optFps            :: Int
  -- ^ How many frames an interactive video should render per second
  , optRenderMode :: RenderMode
  -- ^ Should the program render a png directly or spawn an interactive video?
  }

getDefaultOpts :: IO Opts
getDefaultOpts = do
  seed <- round . (* 1000) <$> getPOSIXTime
  pure Opts
    { optSeed           = Just seed
    , optScale          = 1
    , optWidth          = 100
    , optHeight         = 100
    , optRenderTimes    = 1
    , optName           = "sketch"
    , optMetadataString = Nothing
    , optFps            = 30
    , optRenderMode     = Interactive
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
    <*> optional (strOption (long "metadata" <> metavar "METADATA"))
    <*> option auto (long "fps" <> metavar "FPS" <> value 30)
    <*> flag Interactive Static (long "static" <> short 's')

optsInfo :: ParserInfo Opts
optsInfo = info
  (opts <**> helper)
  (fullDesc <> progDesc "Generate art with ChaosBox" <> header "chaosbox")

-- | Run 'ChaosBox' with 'Opts' parsed from the CLI
--
-- For the following program (assume it's called @sketch@):
--
-- @
-- main = runChaosBox generate
--
-- generate :: Generate ()
-- generate = -- blah blah blah
-- @
--
-- Run @sketch --help@ to view all available options.
--
runChaosBox
  :: Generate a
  -- ^ Render function
  -> IO ()
runChaosBox = runChaosBoxWith id

-- | Run 'ChaosBox' with 'Opts' parsed from the CLI, allowing overrides.
--
-- You might want to parse some options from the CLI, but not all of them. For
-- example, it's relatively common to fix 'optHeight' and 'optWidth', but vary
-- other factors. 'runChaosBoxWith' allows this flexibility:
--
-- @
-- runChaosBoxWith (opts -> opts { optWidth = 16, optHeight = 20)) generate
-- @
--
runChaosBoxWith
  :: (Opts -> Opts)
  -- ^ Option modifier
  -> Generate a
  -- ^ Render function
  -> IO ()
runChaosBoxWith fn render = do
  options <- execParser optsInfo
  runChaosBoxDirectly (fn options) render

-- | Run 'ChaosBox' with the given 'Opts'
--
-- Does not produce a CLI interface, just an executable with no arguments or
-- options.
--
runChaosBoxDirectly
  :: Opts
  -- ^ Art options
  -> Generate a
  -- ^ Render function
  -> IO ()
runChaosBoxDirectly Opts {..} doRender = replicateM_ optRenderTimes $ do
  seed <- case optSeed of
    Just seed' -> pure seed'
    Nothing    -> round . (* 1000) <$> getPOSIXTime

  let stdGen = pureMT seed
      w      = round $ fromIntegral optWidth * optScale
      h      = round $ fromIntegral optHeight * optScale

  (surface, window) <- case optRenderMode of
    Static      -> (,) <$> createImageSurface FormatARGB32 w h <*> pure Nothing
    Interactive -> do
      SDL.initialize [SDL.InitVideo]
      window <- SDL.createWindow
        "ChaosBox"
        SDL.defaultWindow
          { SDL.windowInitialSize = V2 (fromIntegral w) (fromIntegral h)
          , SDL.windowHighDPI     = True
          }
      SDL.showWindow window
      screenSurface <- SDL.getWindowSurface window
      let white = V4 maxBound maxBound maxBound maxBound
      SDL.surfaceFillRect screenSurface Nothing white
      pixels  <- castPtr <$> surfacePixels screenSurface

      surface <- createImageSurfaceForData pixels FormatRGB24 w h (w * 4)
      pure (surface, Just window)

  progressRef         <- newIORef 0
  beforeSaveHookRef   <- newIORef Nothing
  lastRenderedTimeRef <- newIORef 0
  gcEventHandlerRef   <- newIORef (EventHandler $ const $ pure ())

  let ctx = GenerateCtx
        { gcWidth          = optWidth
        , gcHeight         = optHeight
        , gcSeed           = seed
        , gcScale          = optScale
        , gcName           = optName
        , gcProgress       = progressRef
        , gcBeforeSaveHook = beforeSaveHookRef
        , gcCairoSurface   = surface
        , gcWindow         = window
        , gcVideoManager   = VideoManager
          { vmFps                 = optFps
          , vmLastRenderedTimeRef = lastRenderedTimeRef
          }
        , gcEventHandler   = gcEventHandlerRef
        , gcMetadataString = optMetadataString
        }

  void . renderWith surface . flip runReaderT ctx . flip runRandT stdGen $ do
    cairo $ scale optScale optScale
    void doRender

    ref   <- asks gcBeforeSaveHook
    mHook <- liftIO $ readIORef ref
    fromMaybe (pure ()) mHook

    saveImage

-- | Save the current image at @./images/$name/$seed@
saveImage :: Generate ()
saveImage = saveImageWith Nothing

-- | Save the current image at @./images/$name/$seed/$random-string@
saveImageWith :: Maybe String -> Generate ()
saveImageWith mStr = do
  GenerateCtx {..} <- ask
  mHook <- readIORef gcBeforeSaveHook
  for_ mHook $ \hook -> hook

  liftIO $ do
    putStrLn "Saving Image"
    createDirectoryIfMissing True $ "./images/" <> gcName
    for_ mStr $ \_ ->
      createDirectoryIfMissing True
        $  "./images/"
        <> gcName
        <> "/"
        <> show gcSeed

    putStrLn "Generating art..."
    let regularFile =
          "images/"
            <> gcName
            <> "/"
            <> show gcSeed
            <> "-"
            <> show gcScale
            <> fromMaybe "" gcMetadataString
            <> ".png"
        latest = "images/" <> gcName <> "/latest.png"

    putStrLn $ "Writing " <> regularFile
    surfaceWriteToPNG gcCairoSurface regularFile

    putStrLn $ "Writing " <> latest
    surfaceWriteToPNG gcCairoSurface latest

    for_ mStr $ \s -> do
      let extraFile =
            "images/"
              <> gcName
              <> "/"
              <> show gcSeed
              <> "/"
              <> show gcSeed
              <> "-"
              <> show gcScale
              <> "-"
              <> s
              <> fromMaybe "" gcMetadataString
              <> ".png"

      putStrLn $ "Writing " <> extraFile
      surfaceWriteToPNG gcCairoSurface extraFile
