module Main where

import           ChaosBox
import           ChaosBox.Geometry.ClosedCurve (closedCurve)
import           Control.Monad                 (replicateM)
import           Control.Monad.Random          (getRandomR)
import           Data.Foldable                 (for_)
import           Linear.V2

main :: IO ()
main = runChaosBoxIOWith (\opts -> opts { optWidth = 400, optHeight = 400 })
                         renderSketch

renderSketch :: Generate ()
renderSketch = do
  let white = RGB 1 1 1
      black = RGB 0 0 0

  cairo $ do
    setLineJoin LineJoinRound
    setLineCap LineCapRound

  fillScreenRGB white

  (w, h)     <- getSize

  randomPath <- replicateM 10 $ V2 <$> getRandomR (20, w - 20) <*> getRandomR
    (20, h - 20)

  for_ (closedCurve randomPath) $ \p -> cairo $ do
    setLineWidth 8
    setSourceRGB black *> draw p *> stroke
