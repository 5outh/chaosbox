# chaosbox

A framework for generative art.

`chaosbox` exposes:

- An easy-to-hook-into CLI interface for generating art (`runChaosBoxIO` and friends),
  see `--help` for details.
- A monad transformer stack (`Generate`) that:
  - is capable of drawing with `cairo`
  - instantiates `MonadRandom`, for functions like `getRandomR` and `uniform`
  - instantiates `random-fu.MonadRandom`, to pull from non-uniform distributions like `normal` and `bernoulli`.
- A modest collection of data types for Shapes that can be drawn to a Cairo canvas
- Some utilities for working with color and noise, that interface well with the `linear` package.

This should provide good starting point for generating art with Haskell.

Example (copied from the example project):

```hs
module Main where

import           ChaosBox
import           Control.Monad        (replicateM)
import           Control.Monad.Random (getRandomR)
import           Data.Foldable        (for_)
import           Linear.V2

main :: IO ()
main = runChaosBoxIOWith (\opts -> opts { optWidth = 400, optHeight = 400 })
                         renderSketch

renderSketch :: Generate ()
renderSketch = do
  let white = RGB 1 1 1
      black = RGB 0 0 0

  fillScreenRGB white

  (w, h)     <- getSize

  randomPath <- replicateM 100 $ V2 <$> getRandomR (0, w) <*> getRandomR (0, h)
  for_ (path randomPath) $ \p -> cairo $ setSourceRGB black *> draw p *> stroke
```

if installing from source, this example can be run with `stack exec chaosbox-example`.

It will generate a path made up of 100 random points within a 400x400px viewport
and draw it in black. Images are saved to an `images` directory in the directory
your executable is run in. To overwrite the subdirectory images are written to,
you can provide the `--name` parameter to your executable. See `--help` for
more options.
