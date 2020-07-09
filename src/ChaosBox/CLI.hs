{-# LANGUAGE OverloadedStrings #-}
-- | CLI entrypoints for @ChaosBox@
--
-- Most @ChaosBox@ programs will start with either 'runChaosBox',
-- 'runChaosBoxWith' or 'runChaosBoxDirectly'. These functions build a
-- command-line interface to start the art generation program.
--
-- For example, the following @main@:
--
-- @
-- main = runChaosBoxWith
--  (\o -> o { optWidth = 10, optHeight = 10, optFps = 60 })
--  renderSketch
-- @
--
-- will spawn the following command-line interface, and override the default or
-- user-supplied @width@, @height@, and @fps@:
--
-- @
-- ChaosBox
--
-- Usage: chaosbox-example [--seed SEED] [--scale SCALE] [-w|--width WIDTH]
--                         [-h|--height HEIGHT] [--times TIMES] [--name NAME]
--                         [--metadata METADATA] [--fps FPS] [-s|--static]
--   Generate art with ChaosBox
--
-- Available options:
--   --seed SEED              Seed for the global PRNG
--   --scale SCALE            Scaling factor from user space to image space
--   -w,--width WIDTH         Width of the canvas in user space
--   -h,--height HEIGHT       Width of the canvas in user space
--   --times TIMES            How many times to run the program
--   --name NAME              Name of the output
--   --metadata METADATA      Optional metadata string to append to output file
--                            name
--   --fps FPS                How many frames per second to render in interactive
--                            mode
--   -s,--static              Render an image directly instead of in an interactive
--                            window
--   -h,--help                Show this help text
-- @
--
module ChaosBox.CLI
  (
  -- * ChaosBox options
    Opts(..)
  , RenderMode(..)
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

import           ChaosBox.Orphanage             ( )
import           Data.Foldable                  ( for_ )
import           Control.Monad.Random
import           Control.Monad.Reader
import           UnliftIO.IORef
import           Data.Char                      ( toLower )
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
  deriving (Eq, Show)

data FileFormat = PNG
                | SVG
                | PS
                | PDF
  deriving (Read, Show, Eq)

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
  , optRenderMode     :: RenderMode
  -- ^ Should the program render a png directly or spawn an interactive video?
  , optFileFormat     :: FileFormat
  -- ^ Which file format to use for saving images in static mode
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
            , optMetadataString = Nothing
            , optFps            = 30
            , optRenderMode     = Interactive
            , optFileFormat     = PNG
            }

opts :: Parser Opts
opts =
  Opts
    <$> optional (option auto $ long "seed" <> metavar "SEED" <> help seedHelp)
    <*> option auto
               (long "scale" <> metavar "SCALE" <> value 1 <> help scaleHelp)
    <*> option
          auto
          (  long "width"
          <> short 'w'
          <> metavar "WIDTH"
          <> value 100
          <> help widthHelp
          )
    <*> option
          auto
          (  long "height"
          <> short 'h'
          <> metavar "HEIGHT"
          <> value 100
          <> help heightHelp
          )
    <*> option auto
               (long "times" <> metavar "TIMES" <> value 1 <> help timesHelp)
    <*> strOption
          (long "name" <> metavar "NAME" <> value "sketch" <> help nameHelp)
    <*> optional
          (strOption
            (long "metadata" <> metavar "METADATA" <> help metadataHelp)
          )
    <*> option auto (long "fps" <> metavar "FPS" <> value 30 <> help fpsHelp)
    <*> flag Interactive Static (long "static" <> short 's' <> help staticHelp)
    <*> option
          auto
          (  long "fileformat"
          <> short 'f'
          <> metavar "FILEFORMAT"
          <> help fileformatHelp
          <> value PNG
          )
 where
  seedHelp     = "Seed for the global PRNG (optional)"
  scaleHelp    = "Scaling factor from user space to image space (default: 1)"
  widthHelp    = "Width of the canvas in user space (default: 100)"
  heightHelp   = "Width of the canvas in user space (default: 100)"
  timesHelp    = "How many times to run the program (default: 1)"
  nameHelp     = "Name of the output (default: \"sketch\")"
  metadataHelp = "Metadata string to append to output file name (optional)"
  fpsHelp =
    "How many frames per second to render in interactive mode (default: 30)"
  staticHelp
    = "Render an image directly instead of in an interactive window (default: False)"
  fileformatHelp
    = "Fileformat used for images rendered in static mode: PNG, SVG, PS or PDF (default: PNG)"

optsInfo :: ParserInfo Opts
optsInfo = info
  (opts <**> helper)
  (fullDesc <> progDesc "Generate art with ChaosBox" <> header "ChaosBox")

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

  let
    ctx = GenerateCtx
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

  let write ff withSurface = flip (writeImage ctx ff) Nothing $ \filePath ->
        withSurface filePath (fromIntegral w) (fromIntegral h)
          $ \imageSurface -> do
              _ <-
                renderWith imageSurface
                . flip runReaderT ctx
                . flip runRandT   stdGen
                $ do
                    cairo $ scale optScale optScale
                    doRender
              surfaceFinish imageSurface

  case (optFileFormat, optRenderMode) of
    (PNG, _) ->
      void
        . renderWith surface
        . flip runReaderT ctx
        . flip runRandT   stdGen
        $ do
            cairo $ scale optScale optScale
            void doRender

            ref   <- asks gcBeforeSaveHook
            mHook <- liftIO $ readIORef ref
            fromMaybe (pure ()) mHook

            saveImage

    (ff@SVG, Static) -> write ff withSVGSurface
    (ff@PS , Static) -> write ff withPSSurface
    (ff@PDF, Static) -> write ff withPDFSurface
    (fileFormat, renderMode) ->
      error
        $  show renderMode
        <> " mode does not support rendering to "
        <> show fileFormat


-- | Save the current image at @./images/$name/$seed@
saveImage :: Generate ()
saveImage = saveImageWith Nothing

-- | Save the current image at @./images/$name/$seed/$random-string@
saveImageWith :: Maybe String -> Generate ()
saveImageWith mStr = do
  ctx@GenerateCtx {..} <- ask
  mHook                <- readIORef gcBeforeSaveHook
  for_ mHook $ \hook -> hook

  let writer = surfaceWriteToPNG gcCairoSurface
  liftIO $ writeImage ctx PNG writer mStr

type Writer = FilePath -> IO ()
writeImage :: GenerateCtx -> FileFormat -> Writer -> Maybe String -> IO ()
writeImage ctx fileFormat writer mStr = do
  let GenerateCtx {..} = ctx
  let dotExtension = '.' : map toLower (show fileFormat)

  putStrLn "Saving Image"
  createDirectoryIfMissing True $ "./images/" <> gcName
  for_ mStr $ \_ ->
    createDirectoryIfMissing True $ "./images/" <> gcName <> "/" <> show gcSeed

  putStrLn "Generating art..."
  let regularFile =
        "images/"
          <> gcName
          <> "/"
          <> show gcSeed
          <> "-"
          <> show gcScale
          <> fromMaybe "" gcMetadataString
          <> dotExtension
      latest = "images/" <> gcName <> "/latest" <> dotExtension

  putStrLn $ "Writing " <> latest
  writer latest

  putStrLn $ "Writing " <> regularFile
  copyFile latest regularFile

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
            <> dotExtension

    putStrLn $ "Writing " <> extraFile
    copyFile latest extraFile
