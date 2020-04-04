# chaosbox

`chaosbox` is a generative art framework. It ties together many well-known
and powerful tools and builds on them to provide an intuitive, extensible
experience developing artwork powered by algorithms and procedural
generation, interactively.

## Setup

### Prerequisites

You'll need `cairo` and `sdl` version 2.0+ to run a `chaosbox` program.

Follow the platform-specific instructions to get these installed:

- [Install `cairo`](https://www.cairographics.org/download)
- [Setup `sdl2` by LazyFoo](http://lazyfoo.net/tutorials/SDL/01_hello_SDL/index.php)

### Installing the library

`chaosbox` uses `stack` as a build tool. This is pre-release software, so
you'll need to point your `stack.yaml` to the right git commit in your
project's root directory. Additionally you'll need a few extra-deps that don't
exist in recent stackage snapshots:

`stack.yaml`

```yaml
extra-deps:
  - random-extras-0.19
  - gtk2hs-buildtools-0.13.5.4
  - gi-cairo-render-0.0.1@sha256:ff2ccc309c021c2c023fa0d380375ef36cff2df93e0c78ed733f052dd1aa9782,3502
  - github: 5outh/chaosbox
    commit: 93093054cfdf2af0f5d72546aada2c5d474b8c27
```

And add the dependency `chaosbox` to your `cabal` (or `package.yaml` if using hpack) file

`package.yaml`

```
dependencies:
  - chaosbox
```

`project.cabal`

```
executable example-project
  -- ... 
  build-depends:
    - chaosbox
```

Then `stack build` your project as normal.

If you run into any issues with these setup instructions, [please file an
issue](https://github.com/5outh/chaosbox/issues).

## Example `chaosbox` Program

Let's take a look at an example program. First, a description of the program:

> A white curve grows from the center of a black canvas. A new control point
> is generated in every frame. The curve is limited to 100 control points;
> once that limit is exceeded, the curve drops points from the end.
> 
> The amount the curve grows is determined by a backing simplex noise field.
> It will grow more quickly in some spots than others.
> 
> The user can interact with the program by clicking and holding the mouse.
> While the mouse is held, the front of the curve slowly interpolates to the
> cursor's location.

There's quite a bit going on. Here's the code:

```haskell
module Main where

import           ChaosBox

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

-- Run this example with
--
-- @
-- > chaosbox-example -- --scale=60
-- @
--
main :: IO ()
main = runChaosBoxWith (\o -> o { optWidth = 10, optHeight = 10 }) renderSketch

setup :: Render ()
setup = setLineWidth 0.02

renderSketch :: Generate ()
renderSketch = do
  cairo setup

  (w, h)           <- getSize
  center           <- getCenterPoint

  startingPoint    <- normal center (P2 (w / 4) (h / 4))
  pathRef          <- newIORef (startingPoint :| [])
  noise            <- newNoise2

  mousePositionRef <- heldMousePosition ButtonLeft

  eventLoop $ do
    nextPath <- modifyIORefM pathRef $ \ps@(p :| _) -> do
      c <- readIORefWith (maybe p (lerp 0.05 p)) mousePositionRef
      let deviation = 0.3 * noise (c / 100)
      nextPoint <- normal c (P2 deviation deviation)
      pure $ unsafeTake 100 $ nextPoint `NE.cons` ps

    fillScreenRGB black
    cairo $ do
      setSourceRGB white
      draw (ClosedCurve nextPath 10) *> stroke

-- | An unsafe version of 'Data.List.NonEmpty.take'
--
-- This will blow up if n < 1, but is perfectly fine for a static value > 1,
-- such as @100@ (at the callsite above).
--
unsafeTake :: Int -> NonEmpty a -> NonEmpty a
unsafeTake n = NE.fromList . NE.take n
```

Note: this example is taken directly from `chaosbox-example`, an executable
shipped along with `chaosbox`. See the haddock documentation for a
link-annotated version of this program.

This short example covers myriad concepts in `chaosbox`. Notably:

- All drawing is done through `libcairo` via `cairo`.
- We can query the world (`getSize` and `getCenterPoint`)
- `chaosbox` supports non-uniform random sampling (`normal`)
- `chaosbox` handles variable mutation using `IORef`s and provides helpers
around common operations (`modifyIORefM`, `forIORef`)
- Interactive `chaosbox` programs run in an _Event Loop_. This gets called
once per frame.
- We can 'draw' various data types, including `ClosedCurve`s, to the canvas.
(see `ChaosBox.Geometry` for more)
- Some common event handling is abstracted away (`heldMousePosition`)

In addition to what is visible here, `eventLoop` also provides a couple
global keybindings:

- Pressing `s` will save the current image at any time.
- Pressing `q` will immediately quit the window and save the image.

More can be added at any time using facilities provided in
`ChaosBox.Interactive`.

To get a feel for how `ChaosBox` works using this documentation, check out
`ChaosBox.CLI`, `ChaosBox.Generate`, `ChaosBox.Interactive`,
`ChaosBox.Geometry` and `ChaosBox.Affine`.

Additionally, it is recommended to get up to speed with `libcairo` and its
haskell bindings. Since the actual drawing is done using `libcairo`,
learning these bindings will allow you to customize your art much more
easily by digging down a level below this high-level interface.

Have fun!
