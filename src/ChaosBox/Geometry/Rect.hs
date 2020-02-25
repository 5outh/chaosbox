{-# LANGUAGE TypeFamilies #-}
module ChaosBox.Geometry.Rect
  ( RectOf(..)
  , Rect
  , rect
  , square
  , toQuad
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           Control.Lens                   ( (+~)
                                                , (&)
                                                , (^.)
                                                )
import           ChaosBox.HasV2
import           ChaosBox.Geometry.Quad
import           ChaosBox.Draw
import           Graphics.Rendering.Cairo
                                         hiding ( Path
                                                , transform
                                                )

-- | A Rectangle
data RectOf a = RectOf
  { rectTopLeft :: a
  , rectW       :: Double
  , rectH       :: Double
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Rect = RectOf (V2 Double)

instance HasV2 a => Affine (RectOf a) where
  type Transformed (RectOf a) = QuadOf a
  transform m = transform m . toQuad

instance HasV2 a => Draw (RectOf a) where
  draw RectOf {..} =
    rectangle rectX rectY rectW rectH
    where
      V2 rectX rectY = rectTopLeft ^. _V2

rect :: a -> Double -> Double -> RectOf a
rect c w h = RectOf c w h

square :: a -> Double -> RectOf a
square c w = RectOf c w w

toQuad :: HasV2 a => RectOf a -> QuadOf a
toQuad RectOf {..} = QuadOf rectTopLeft
                            (rectTopLeft & _V2 +~ V2 rectW 0)
                            (rectTopLeft & _V2 +~ V2 rectW rectH)
                            (rectTopLeft & _V2 +~ V2 0 rectH)
