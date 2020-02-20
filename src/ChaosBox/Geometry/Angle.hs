module ChaosBox.Geometry.Angle
  ( Angle(..)
  , fromRadians
  , fromDegrees
  , unit
  )
where

import           Data.Fixed (mod')
import           Linear.V2

newtype Angle = Angle { getAngle :: Double }
  deriving (Eq, Ord, Show)

-- | Construct an angle in radians in the range [0, 2*pi)
--
-- Modular arithmetic is used to limit angles out of range
--
fromRadians :: Double -> Angle
fromRadians theta = Angle $ theta `mod'` (2 * pi)

-- | Construct an angle in degrees in the range [0, 360)
--
-- Modular arithmetic is used to limit angles out of range
--
fromDegrees :: Double -> Angle
fromDegrees theta = fromRadians $ theta * pi / 180

-- | The unit vector in the direction of 'Angle'
unit :: Angle -> V2 Double
unit (Angle r) = angle r
