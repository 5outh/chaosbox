module ChaosBox.Color
  (
    -- * HSV Values (Hue-Saturation-Value)
    HSV(..)
  , WithAlpha(..)
  , setSourceHSV
  , setSourceHSVA

    -- * RGB Values (Red-Green-Blue)
  , rgbFromHex
  , setSourceRGB
  , rgb255
  , grayscale
  , black
  , white

    -- * Conversion functions
  , toHSV
  , toRGB

  -- * Context-sensitive actions
  , fillScreenHSV
  , fillScreenRGB
  , module X
  )
where

import           ChaosBox.Generate

import           Data.Colour.RGBSpace     as X
import           Data.Colour.RGBSpace.HSV
import           Data.Colour.SRGB
import           Graphics.Rendering.Cairo hiding (setSourceRGB)
import qualified Graphics.Rendering.Cairo as Cairo

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
setSourceHSV = setSourceRGB . toRGB

setSourceHSVA :: WithAlpha HSV -> Render ()
setSourceHSVA (WithAlpha HSV {..} alpha) =
  hsva hsvHue hsvSaturation hsvValue alpha

rgbFromHex :: String -> RGB Double
rgbFromHex = toSRGB . sRGB24read

setSourceRGB :: RGB Double -> Render ()
setSourceRGB (RGB r g b) = Cairo.setSourceRGB r g b

rgb255 :: Fractional a => a -> a -> a -> RGB a
rgb255 r g b = RGB (r / 255) (g / 255) (b / 255)

toHSV :: RGB Double -> HSV
toHSV rgb = let (h, s, v) = hsvView rgb in HSV h s v

grayscale :: Num a => a -> RGB a
grayscale v = RGB v v v

black :: Num a => RGB a
black = grayscale 0

white :: Num a => RGB a
white = grayscale 1

toRGB :: HSV -> RGB Double
toRGB HSV {..} = hsv2rgb hsvHue hsvSaturation hsvValue

hsv2rgb :: RealFrac a => a -> a -> a -> RGB a
hsv2rgb h s v = case i `mod` 6 of
  0 -> RGB v t p
  1 -> RGB q v p
  2 -> RGB p v t
  3 -> RGB p q v
  4 -> RGB t p v
  5 -> RGB v p q
  _ -> error "mod 6 returned something out of range"
 where
  i :: Int
  i = floor $ h * 6
  f = h * 6 - fromIntegral i
  p = v * (1 - s)
  q = v * (1 - f * s)
  t = v * (1 - (1 - f) * s)

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
  where RGB {..} = hsv h s v

-- Utility function

fillScreenHSV :: HSV -> Generate ()
fillScreenHSV color = do
  (w, h) <- getSize
  cairo $ do
    rectangle 0 0 w h
    setSourceHSV color *> fill

fillScreenRGB :: RGB Double -> Generate ()
fillScreenRGB color = do
  (w, h) <- getSize
  cairo $ do
    rectangle 0 0 w h
    setSourceRGB color *> fill
