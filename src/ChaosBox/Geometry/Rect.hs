{-# LANGUAGE TypeFamilies #-}
module ChaosBox.Geometry.Rect
  ( Rect(..)
  -- , rect
  , square
  , toQuad
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Geometry.Quad
import           ChaosBox.Draw
import           Graphics.Rendering.Cairo
                                         hiding ( Path
                                                , transform
                                                )

-- | A Rectangle
data Rect = Rect
  { rectTopLeft :: V2 Double
  , rectW       :: Double
  , rectH       :: Double
  } deriving (Show, Eq, Ord)

instance Affine Rect where
  type Transformed Rect = Quad
  transform m = transform m . toQuad

instance Draw Rect where
  draw Rect {..} =
    rectangle rectX rectY rectW rectH
    where V2 rectX rectY = rectTopLeft

square :: V2 Double -> Double -> Rect
square c w = Rect c w w

toQuad :: Rect -> Quad
toQuad Rect {..} = Quad rectTopLeft
                        (rectTopLeft + V2 rectW 0)
                        (rectTopLeft + V2 rectW rectH)
                        (rectTopLeft + V2 0 rectH)
