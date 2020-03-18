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

import           Data.Colour.RGBSpace          as X
import           Data.Colour.RGBSpace.HSV
import           Data.Colour.SRGB
import           GI.Cairo.Render         hiding ( setSourceRGB )
import qualified GI.Cairo.Render               as Cairo

-- | Hue-saturation-value color space
data HSV = HSV
  { hsvHue        :: Double
  , hsvSaturation :: Double
  , hsvValue      :: Double
  } deriving (Show, Read, Eq, Ord)

-- | A color with transparency
data WithAlpha color = WithAlpha
  { waColor :: color
  , waAlpha :: Double
  } deriving (Show, Read, Eq, Ord)

-- | Set the current source to an 'HSV' color
setSourceHSV :: HSV -> Render ()
setSourceHSV = setSourceRGB . toRGB

-- | Set the current source to an 'HSVA' color
setSourceHSVA :: WithAlpha HSV -> Render ()
setSourceHSVA (WithAlpha HSV {..} alpha) =
  hsva hsvHue hsvSaturation hsvValue alpha

-- | Parse an RGB value from hexadecimal form
--
-- prop> rgbFromHex "ffffff" = RGB 1 1 1
--
rgbFromHex :: String -> RGB Double
rgbFromHex = toSRGB . sRGB24read

-- | Set the current source to an RGB color
setSourceRGB :: RGB Double -> Render ()
setSourceRGB (RGB r g b) = Cairo.setSourceRGB r g b

-- | Construct an 'RGB' value from components in the range (0,255)
rgb255 :: Fractional a => a -> a -> a -> RGB a
rgb255 r g b = RGB (r / 255) (g / 255) (b / 255)

-- | Convert an 'RGB' value to 'HSV'
toHSV :: RGB Double -> HSV
toHSV rgb = let (h, s, v) = hsvView rgb in HSV h s v

-- | Grayscale with some value between 0 and 1
--
-- prop> grayscale 0 = black
-- prop> grayscale 1 = white
--
grayscale :: Num a => a -> RGB a
grayscale v = RGB v v v

black :: Num a => RGB a
black = grayscale 0

white :: Num a => RGB a
white = grayscale 1

-- | Convert an 'HSV' value to 'RGB'
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

-- | Fill the whole window with an 'HSV' color
fillScreenHSV :: HSV -> Generate ()
fillScreenHSV color = do
  (w, h) <- getSize
  cairo $ do
    rectangle 0 0 w h
    setSourceHSV color *> fill

-- | Fill the whole window with an 'RGB' color
fillScreenRGB :: RGB Double -> Generate ()
fillScreenRGB color = do
  (w, h) <- getSize
  cairo $ do
    rectangle 0 0 w h
    setSourceRGB color *> fill
