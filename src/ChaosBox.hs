-- | The Main "ChaosBox" module
--
-- "ChaosBox" is a generative art framework. It ties together many well-known
-- and powerful tools and builds on them to provide an intuitive, extensible
-- experience developing artwork powered by algorithms and procedural
-- generation, interactively.
--
-- Let's take a look at an example program. First, a description of the program:
--
-- A white curve grows from the center of a black canvas. A new control point
-- is generated in every frame. The curve is limited to 100 control points;
-- once that limit is exceeded, the curve drops points from the end.
--
-- The amount the curve grows is determined by a backing simplex noise field.
-- It will grow more quickly in some spots than others.
--
-- The user can interact with the program by clicking and holding the mouse.
-- While the mouse is held, the front of the curve slowly interpolates to the
-- cursor's location.
--
-- There's quite a bit going on. Here's the code:
--
-- @
-- import           ChaosBox
--
-- import           Data.List.NonEmpty (NonEmpty (..))
-- import qualified Data.List.NonEmpty as NE
--
-- main :: IO ()
-- main = 'runChaosBoxWith' (\o -> o { 'optWidth' = 10, 'optHeight' = 10, 'optScale' = 60 }) 'renderSketch'
--
-- renderSketch :: Generate ()
-- renderSketch = do
--   'cairo' setup
--
--   (w, h)           <- 'getSize'
--   center           <- 'getCenterPoint'
--
--   startingPoint    <- 'normal' center \$ 'P2' (w \/ 4) (h \/ 4)
--   pathRef          <- 'newIORef' \$ startingPoint :| []
--   noise            <- 'newNoise2'
--
--   mousePositionRef <- 'heldMousePosition' 'ButtonLeft'
--
--   'eventLoop' $ do
--     nextPath \<- 'modifyIORefM' pathRef \$ \ps@(p :| _) -\> do
--       c <- 'forIORef' mousePositionRef \$ maybe p ('lerp' 0.05 p)
--       let deviation = 0.3 * noise (c / 100)
--       nextPoint <- 'normal' c $ 'P2' deviation deviation
--       pure $ unsafeTake 100 $ nextPoint \`NE.cons\` ps
--
--     'fillScreenRGB' 'black'
--     'cairo' $ do
--       'setSourceRGB' 'white'
--       'draw' ('ClosedCurve' nextPath 2) *> 'stroke'
--
-- -- setup :: 'Render' ()
-- -- setup = 'setLineWidth' 0.02
--
-- -- | An unsafe version of 'Data.List.NonEmpty.take'
-- --
-- unsafeTake :: Int -> NonEmpty a -> NonEmpty a
-- unsafeTake n = NE.fromList . NE.take n
-- @
--
-- This example has been annotated with links in order to make exploring its
-- functionality easy. This short example covers myriad concepts in "ChaosBox".
-- Notably:
--
-- - All drawing is done through @libcairo@ via 'cairo'.
-- - We can query the world ('getSize' and 'getCenterPoint')
-- - "ChaosBox" supports non-uniform random sampling ('normal')
-- - "ChaosBox" handles variable mutation using 'IORef's and provides helpers
-- around common operations ('modifyIORefM', 'forIORef')
-- - Interactive "ChaosBox" programs run in an /Event Loop/. This gets called
-- once per frame.
-- - We can 'draw' various data types, including 'ClosedCurve's, to the canvas.
-- (see "ChaosBox.Geometry" for more)
-- - Some common event handling is abstracted away ('heldMousePosition')
--
-- In addition to what is visible here, 'eventLoop' also provides a couple
-- global keybindings:
--
-- - Pressing 's' will save the current image at any time.
-- - Pressing 'q' will immediately quit the window and save the image.
--
-- More can be added at any time using facilities provided in
-- "ChaosBox.Interactive".
--
-- To get a feel for how "ChaosBox" works using this documentation, check out
-- "ChaosBox.CLI", "ChaosBox.Generate", "ChaosBox.Interactive",
-- "ChaosBox.Geometry" and "ChaosBox.Affine".
--
-- Additionally, it is recommended to get up to speed with @libcairo@ and its
-- haskell bindings. Since the actual drawing is done using @libcairo@,
-- learning these bindings will allow you to customize your art much more
-- easily by digging down a level below this high-level interface.
--
-- Have fun!
--
module ChaosBox
  ( module X
    -- * Re-exports
  , module Linear.V2
  , module UnliftIO.IORef
  , module Ext
  )
where

import           ChaosBox.AABB        as X
import           ChaosBox.CLI         as X
import           ChaosBox.Color       as X
import           ChaosBox.Draw        as X
import           ChaosBox.Generate    as X
import           ChaosBox.Geometry    as X
import           ChaosBox.Interactive as X
import           ChaosBox.Math        as X
import           ChaosBox.Noise       as X
import           ChaosBox.PNG         as X
import           ChaosBox.Random      as X
import           GI.Cairo.Render      as Ext (LineCap (..), LineJoin (..),
                                              Render, fill, fillPreserve,
                                              setLineCap, setLineJoin,
                                              setLineWidth, stroke,
                                              strokePreserve)
import           Linear.V2
import           UnliftIO.IORef
