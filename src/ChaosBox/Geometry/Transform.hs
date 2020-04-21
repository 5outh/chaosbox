module ChaosBox.Geometry.Transform
  ( translatePoints
  , scalePoints
  , scaleAroundPoints
  , rotatePoints
  , rotateAroundPoints
  )
where

import           ChaosBox.Geometry.Angle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           Control.Lens            ((%~))

translatePoints :: (Functor f, HasP2 a) => P2 -> f a -> f a
translatePoints p2 = fmap (_V2 %~ translateP2 p2)

scalePoints :: (Functor f, HasP2 a) => P2 -> f a -> f a
scalePoints amount = fmap (_V2 %~ scaleP2 amount)

scaleAroundPoints :: (Functor f, HasP2 a) => P2 -> P2 -> f a -> f a
scaleAroundPoints center amount = fmap (_V2 %~ scaleP2Around center amount)

rotatePoints :: (Functor f, HasP2 a) => Angle -> f a -> f a
rotatePoints theta = fmap (_V2 %~ rotateP2 theta)

rotateAroundPoints :: (Functor f, HasP2 a) => P2 -> Angle -> f a -> f a
rotateAroundPoints center theta = fmap (_V2 %~ rotateP2Around center theta)
