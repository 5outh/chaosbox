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
  c          <- getCenterPoint

  randomPath <- replicateM 100 $ V2 <$> getRandomR (0, w) <*> getRandomR (0, h)
  for_ (path randomPath) $ \p -> cairo $ setSourceRGB black *> draw p *> stroke

  let e = ellipse c (w / 10) (h / 3)

  cairo $ draw e *> stroke
