module ChaosBox.Color where

import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import           Graphics.Rendering.Cairo

data HSV = HSV
  { hsvHue        :: Double
  , hsvSaturation :: Double
  , hsvValue      :: Double
  } deriving (Show, Eq)

data WithAlpha color = WithAlpha
  { waColor :: color
  , waAlpha :: Double
  }

setSourceHSV :: HSV -> Render ()
setSourceHSV hsv = setSourceHsva (hsv `WithAlpha` 1)

setSourceHSVA :: WithAlpha HSV -> Render ()
setSourceHSVA (WithAlpha HSV {..} alpha) =
  hsva hsvHue hsvSaturation hsvValue alpha

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
  where RGB {..} = hsv h s v
