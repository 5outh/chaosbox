module ChaosBox.Geometry.Rect
  ( Rect(..)
  , rect
  , square
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           Graphics.Rendering.Cairo
                                         hiding ( Path )

-- | A Rectangle
data Rect = Rect
  { rectTopLeft :: V2 Double
  , rectW       :: Double
  , rectH       :: Double
  , rectMatrix  :: M33 Double
  } deriving (Show, Eq, Ord)

rect :: V2 Double -> Double -> Double -> Rect
rect tl w h = Rect tl w h identity

instance Affine Rect where
  matrixLens wrap (Rect tl w h m) = fmap (Rect tl w h) (wrap m)

instance Draw Rect where
  draw Rect {..} = withCairoAffine rectMatrix
    $ rectangle rectX rectY rectW rectH
    where V2 rectX rectY = rectTopLeft

-- instance HasCenter Rect where
  -- getCenter Rect {..} = average [rectTopLeft, V2 rectW rectH]

square :: V2 Double -> Double -> Rect
square c w = Rect c w w identity
