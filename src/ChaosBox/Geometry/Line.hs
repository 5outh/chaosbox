module ChaosBox.Geometry.Line
  ( Line(..)
  , line
  , bakeLine
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           Data.List.NonEmpty
import           ChaosBox.Geometry.Path

-- | A line segment
data Line = Line
  { lineStart  :: V2 Double
  , lineEnd    :: V2 Double
  , lineMatrix :: M33 Double
  } deriving (Show, Eq, Ord)

line :: V2 Double -> V2 Double -> Line
line s e = Line s e identity

instance Affine Line where
  matrixLens wrap (Line s e m) = fmap (Line s e) (wrap m)

instance Draw Line where
  draw Line {..} = draw $ Path (lineStart :| [lineEnd]) lineMatrix

bakeLine :: Line -> Line
bakeLine l@(Line s e _) = Line (applyAffine l s) (applyAffine l e) identity
