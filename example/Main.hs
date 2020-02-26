module Main where

import           ChaosBox

import           Control.Monad (replicateM)
import           Data.Foldable (for_)

-- Run this example with
--
-- @@@
-- > chaosbox-example -- --scale=100
-- @@@
--
main :: IO ()
main = runChaosBoxIOWith (\opts -> opts { optWidth = 8, optHeight = 10 })
                         renderSketch

renderSketch :: Generate ()
renderSketch = do
  setup

  (w, h)     <- getSize

  center     <- getCenterPoint
  randomPath <- replicateM 20 $ normal center $ V2 (w / 4) (h / 4)

  cairo $ for_ (closedCurve randomPath) $ \p -> draw p *> stroke

setup :: Generate ()
setup = do
  cairo $ do
    setLineWidth 0.05
    setLineJoin LineJoinRound
    setLineCap LineCapRound

  fillScreenRGB white

  cairo $ setSourceRGB black
