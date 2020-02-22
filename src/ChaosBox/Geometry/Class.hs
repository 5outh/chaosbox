{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}
module ChaosBox.Geometry.Class
  ( HasCenter(..)
  , Scale(..)
  , Translate(..)
  , Rotate(..)
  , Geometry
  , scale
  , rotate
  )
where

import           ChaosBox.Geometry.Angle
import           Linear.Matrix           ((!*))
import           Linear.V2

class HasCenter a where
  getCenter :: a -> V2 Double

class Scale a where
  type Scaled a
  type Scaled a = a
  scaleAround
    :: V2 Double
    -- ^ Center
    -> V2 Double
    -- ^ Scale
    -> a
    -> Scaled a

class Translate a where
  translate :: V2 Double -> a -> a

class Rotate a where
  type Rotated a
  type Rotated a = a
  rotateAround :: V2 Double -> Angle -> a -> Rotated a

type Geometry a = (HasCenter a, Scale a, Translate a, Rotate a)

-- V2 instances

instance a ~ Double => HasCenter (V2 a) where
  getCenter = id

instance a ~ Double => Scale (V2 a) where
  scaleAround c s p = c + (s * (p - c))

instance a ~ Double => Translate (V2 a) where
  translate = (+)

instance a ~ Double => Rotate (V2 a) where
  rotateAround center (Angle theta) v =
    center + (rotationMatrix !* (v - center))
   where
    rotationMatrix =
      V2 (V2 (cos theta) (-(sin theta))) (V2 (sin theta) (cos theta))

-- Utilities

scale :: (HasCenter a, Scale a) => V2 Double -> a -> Scaled a
scale s a = scaleAround (getCenter a) s a

rotate :: (HasCenter a, Rotate a) => Angle -> a -> Rotated a
rotate theta a = rotateAround (getCenter a) theta a
