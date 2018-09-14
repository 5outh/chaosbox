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
