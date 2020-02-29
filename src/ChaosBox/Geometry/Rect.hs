{-# LANGUAGE TypeFamilies #-}
module ChaosBox.Geometry.Rect
  ( RectOf(..)
  , Rect
  -- * Smart constructors
  , rect
  , square
  -- * Conversions
  , fromAABB
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Geometry.P2
import           ChaosBox.AABB
import           ChaosBox.Geometry.Class
import           ChaosBox.Draw
import           Control.Lens                   ( (^.) )
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

type Rect = RectOf P2

instance HasP2 a => HasAABB (RectOf a) where
  aabb (RectOf tl w h) =  AABB (tl ^. _V2) w h

instance HasP2 a => Draw (RectOf a) where
  draw RectOf {..} = rectangle rectX rectY rectW rectH
    where V2 rectX rectY = rectTopLeft ^. _V2

instance HasP2 a => Boundary (RectOf a) where
  RectOf{..} `containsPoint` (V2 x y) = x >= tlx && x < brx && y >= tly && y <= bry
   where
    V2 tlx tly = rectTopLeft ^. _V2
    V2 brx bry = rectTopLeft ^. _V2 + V2 rectW rectH

rect :: a -> Double -> Double -> RectOf a
rect c w h = RectOf c w h

square :: a -> Double -> RectOf a
square c w = RectOf c w w

fromAABB :: AABB -> Rect
fromAABB (AABB tl w h) = RectOf tl w h
