-- | The Main @ChaosBox@ module
--
-- @ChaosBox@ is a generative art framework. It ties together many well-known
-- and powerful tools and builds on them to provide an intuitive, extensible
-- experience developing artwork powered by algorithms and procedural
-- generation, interactively.
--
-- Let's take a look at an example:
--
module ChaosBox
  ( module X
    -- * Re-exports
  , module Ext
  )
where

import           ChaosBox.AABB        as X
import           ChaosBox.Affine      as X
import           ChaosBox.CLI         as X
import           ChaosBox.Color       as X
import           ChaosBox.Draw        as X
import           ChaosBox.Generate    as X
import           ChaosBox.Geometry    as X
import           ChaosBox.Interactive as X
import           ChaosBox.Math        as X
import           ChaosBox.Noise       as X
import           ChaosBox.Pixel       as X
import           ChaosBox.Random      as X
import           GI.Cairo.Render      as Ext (LineCap (..), LineJoin (..),
                                              Render, fill, fillPreserve,
                                              setLineCap, setLineJoin,
                                              setLineWidth, stroke,
                                              strokePreserve)
import           Linear.V2            as Ext
import           UnliftIO.IORef       as Ext (IORef, modifyIORef, newIORef,
                                              readIORef, writeIORef)
