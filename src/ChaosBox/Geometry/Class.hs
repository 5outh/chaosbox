{-# LANGUAGE MultiParamTypeClasses #-}
module ChaosBox.Geometry.Class
  ( Boundary(..)
  , HasP2(..)
  , Intersects(..)
  , getP2
  , setP2
  , modifyP2
  )
where

import           ChaosBox.Geometry.P2
import           Control.Lens         (Lens', lens, (%~), (&), (.~), (^.))
import           Data.Complex
import           Linear.V2

-- | Class of objects that can be queried for points
class Boundary a where
  containsPoint :: a -> P2 -> Bool

class HasP2 a where
  _V2 :: Lens' a P2

instance HasP2 P2 where
  _V2 = _xy

instance HasP2 (Complex Double) where
  _V2 = lens (\(a :+ b) -> V2 a b) (\_ (V2 x y) -> x :+ y)

getP2 :: HasP2 a => a -> P2
getP2 = (^. _V2)

setP2 :: HasP2 a => a -> P2 -> a
setP2 x p2 = x & _V2 .~ p2

modifyP2 :: HasP2 a => a -> (P2 -> P2) -> a
modifyP2 x f = x & _V2 %~ f

class Intersects a b where
  intersectionPoints :: a -> b -> [P2]
  intersects :: a -> b -> Bool
  intersects a b = null (intersectionPoints a b)
