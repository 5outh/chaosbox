{-# LANGUAGE TypeFamilies #-}
module ChaosBox.HasV2
  ( HasV2(..)
  )
where

import           Control.Lens (Lens', lens)
import           Data.Complex
import           Linear.V2

class HasV2 a where
  _V2 :: Lens' a (V2 Double)

instance HasV2 (V2 Double) where
  _V2 = _xy

instance HasV2 (Complex Double) where
  _V2 = lens (\(a :+ b) -> V2 a b) (\_ (V2 x y) -> x :+ y)
