module ChaosBox.Geometry.Class
  ( Boundary(..)
  , HasV2(..)
  )
where

import           ChaosBox.Geometry.P2
import           Control.Lens         (Lens', lens)
import           Data.Complex
import           Linear.V2

-- | Class of objects that can be queried for points
class Boundary a where
  containsPoint :: a -> P2 -> Bool

class HasV2 a where
  _V2 :: Lens' a P2

instance HasV2 P2 where
  _V2 = _xy

instance HasV2 (Complex Double) where
  _V2 = lens (\(a :+ b) -> V2 a b) (\_ (V2 x y) -> x :+ y)
