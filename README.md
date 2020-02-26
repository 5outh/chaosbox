# chaosbox

A framework for generative art.

`chaosbox` exposes:

- Functions to easily build a CLI interface for generating art (`runChaosBoxIO` and friends), see `--help` for details.
- A monad transformer stack (`Generate`) that:
  - is capable of drawing to a png image file using `cairo`
  - instantiates `MonadRandom`, for functions like `getRandomR` and `uniform`
  - instantiates `random-fu.MonadRandom`, to pull from non-uniform distributions like `normal` and `bernoulli`.
- A library of Random generation (`ChaosBox.Random`)
- A collection of data types for Shapes that can be drawn
- Some utilities for working with color and noise, that interface well with the `linear` package.

Simple Example (copied from the example project):

```hs
import           ChaosBox

import           Control.Monad (replicateM)
import           Data.Foldable (for_)

-- Run this example with
--
-- @@@
-- > chaosbox-example -- --scale=100
-- @@@
--
-- to generate a 800x1000 px version of this artwork
--
main :: IO ()
main = runChaosBoxIOWith (\opts -> opts { optWidth = 8, optHeight = 10 })
                         renderSketch

renderSketch :: Generate ()
renderSketch = do
  setup

  (w, h)     <- getSize

  center     <- getCenterPoint
  randomPath <- replicateM 10 $ normal center $ V2 (w / 4) (h / 4)

  cairo $ for_ (closedCurve randomPath) $ \p -> draw p *> stroke

setup :: Generate ()
setup = do
  cairo $ do
    setLineWidth 0.05
    setLineJoin LineJoinRound
    setLineCap LineCapRound

  fillScreenRGB white

  cairo $ setSourceRGB black
```

if installing from source, this example can be run with `stack exec chaosbox-example`.

It will generate a path made up of 100 random points within a 400x400px viewport
and draw it in black. Images are saved to an `images` directory in the directory
your executable is run in. To overwrite the subdirectory images are written to,
you can provide the `--name` parameter to your executable. See `--help` for
more options.
