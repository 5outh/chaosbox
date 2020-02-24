module Main where

import           ChaosBox
-- import           ChaosBox.Affine
import qualified ChaosBox.Geometry.ClosedCurve as ClosedCurve
-- import qualified ChaosBox.Geometry.Curve       as Curve
-- import           ChaosBox.Math.Matrix
import           Control.Monad                 (replicateM)
import           Control.Monad.Random          (getRandomR)
import           Data.Foldable                 (for_)
-- import           Data.List                     (sort)
-- import           Linear.Matrix                 ((!*!))
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
  for_ (ClosedCurve.closedCurve randomPath) $ \p -> do
    cairo $ setLineWidth 8
    cairo $ setSourceRGB black *> draw p *> stroke
