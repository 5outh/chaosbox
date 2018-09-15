# chaosbox

A minimal framework for generative art.

`chaosbox` exposes:

- An easy-to-hook-into CLI interface for generating art (`runChaosBoxIO` and friends),
  see `--help` for details.
- A monad transformer stack (`Generate`) that:
  - is capable of drawing with `cairo`
  - instantiates `MonadRandom`, for functions like `getRandomR` and `uniform`
  - instantiates `random-fu.MonadRandom`, to pull from non-uniform distributions like `normal` and `bernoulli`.

It's very simple and doesn't try to do much other than this. The goal is to
provide an absolutely minimal shim for those who want a pre-built framework for
creating images from random data.

Example (copied from the example project):

```hs
module Main where

import           ChaosBox
import           Control.Monad            (replicateM)
import           Control.Monad.Random     (getRandomR)
import           Data.Foldable            (for_)
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty       as NE
import           Graphics.Rendering.Cairo
import           Linear.V2

main :: IO ()
main = runChaosBoxIOWith (\opts -> opts { optWidth = 400, optHeight = 400 })
                         renderSketch

renderSketch :: Generate ()
renderSketch = do
  let white = HSV 0 0 1
      black = HSV 0 0 0

  fillScreenHSV white

  (w, h)     <- getSize

  randomPath <- replicateM 100 $ V2 <$> getRandomR (0, w) <*> getRandomR (0, h)

  cairo $ do
    setLineWidth 1
    setSourceHSV black
    path (NE.fromList randomPath) *> stroke

path :: NE.NonEmpty (V2 Double) -> Render ()
path ((V2 startX startY):|rest) = do
  newPath
  moveTo startX startY
  for_   rest   (\(V2 x y) -> lineTo x y)
```

if installing from source, this example can be run with `stack exec chaosbox-example`.

It will generate a path made up of 100 random points within a 400x400px viewport
and draw it in black. Images are saved to an `images` directory in the directory
your executable is run in. To overwrite the subdirectory images are written to,
you can provide the `--name` parameter to your executable. See `--help` for
more options.
