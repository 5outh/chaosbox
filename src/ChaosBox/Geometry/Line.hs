module ChaosBox.Geometry.Line
  ( Line(..)
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Math.Matrix           ( applyMatrix )
import           ChaosBox.Affine
import           ChaosBox.Draw
import           Data.List.NonEmpty
import           ChaosBox.Geometry.Path

-- | A line segment
data Line = Line
  { lineStart  :: V2 Double
  , lineEnd    :: V2 Double
  } deriving (Show, Eq, Ord)

instance Affine Line where
  transform m Line{..} = Line
    (applyMatrix m lineStart)
    (applyMatrix m lineEnd)

instance Draw Line where
  draw Line {..} = draw $ Path (lineStart :| [lineEnd])
