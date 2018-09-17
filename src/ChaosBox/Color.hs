module ChaosBox.Color (HSV(..),WithAlpha(..),setSourceHSV,setSourceHSVA) where

import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import           Graphics.Rendering.Cairo

data HSV = HSV
  { hsvHue        :: Double
  , hsvSaturation :: Double
  , hsvValue      :: Double
  } deriving (Show, Read, Eq, Ord)

data WithAlpha color = WithAlpha
  { waColor :: color
  , waAlpha :: Double
  } deriving (Show, Read, Eq, Ord)

setSourceHSV :: HSV -> Render ()
setSourceHSV color = setSourceHSVA (color `WithAlpha` 1)

setSourceHSVA :: WithAlpha HSV -> Render ()
setSourceHSVA (WithAlpha HSV {..} alpha) =
  hsva hsvHue hsvSaturation hsvValue alpha

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
  where RGB {..} = hsv h s v
