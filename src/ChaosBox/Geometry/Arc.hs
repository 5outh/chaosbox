module ChaosBox.Geometry.Arc
  ( Arc(..)
  , arc
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Geometry.Angle
import           ChaosBox.Affine
import           ChaosBox.Draw
import           Graphics.Rendering.Cairo
                                         hiding ( Path )

-- | Arc (partial Circle)
data Arc = Arc
  { arcCenter :: V2 Double
  -- ^ Center of the arc's circle
  , arcRadius :: Double
  -- ^ Radius of the arc's circle
  , arcStart  :: Angle
  -- ^ Start 'Angle'
  , arcEnd    :: Angle
  -- ^ End 'Angle'
  , arcMatrix :: M33 Double
  } deriving (Eq, Ord, Show)

instance Draw Arc where
  draw Arc {..} = withCairoAffine arcMatrix
    $ arc x y arcRadius (getAngle arcStart) (getAngle arcEnd)
    where V2 x y = arcCenter

-- TODO: arcPoints, instance affine
