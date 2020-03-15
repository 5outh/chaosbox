-- | Angles
module ChaosBox.Geometry.Angle
  ( Angle(..)
  , fromRadians
  , fromDegrees
  , unit
  , addRadians
  , addDegrees
  )
where

import           ChaosBox.Geometry.P2
import           Data.Fixed                     ( mod' )
import           Linear.V2

newtype Angle = Angle { getAngle :: Double }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional, Enum)

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
unit :: Angle -> P2
unit (Angle r) = angle r

-- | Add radians to an 'Angle'
--
-- Note: This is allowed to break the bounds of 0..2*pi radians
--
addRadians :: Double -> Angle -> Angle
addRadians theta (Angle a) = Angle (theta + a)

-- | Add radians to an 'Angle'
--
-- Note: This is allowed to break the bounds of 0..360 degrees
--
addDegrees :: Double -> Angle -> Angle
addDegrees theta (Angle a) = Angle (theta * pi / 180 + a)
