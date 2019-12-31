{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module ChaosBox.Pixel
  ( parsePixelsFromFile
  , getPixelAt
  , getColumn
  , getRow
  , PixelArray(..)
  , Pixel(..)
  )
where

import           ChaosBox.Color           (rgb255)

import           Data.Array.MArray        (readArray)
import           Data.Bits
import           Data.Colour.RGBSpace
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq
import           Data.Traversable
import           Data.Word
import           Graphics.Rendering.Cairo hiding (Path)
import           Linear.V2

newtype Pixel = Pixel { getPixel :: RGB Double }
 deriving (Show, Eq)

newtype PixelArray = PixelArray{ getPixelArray :: Seq (Seq Pixel) }
 deriving (Show, Eq)

getPixelAt :: V2 Int -> PixelArray -> Pixel
getPixelAt (V2 x y) pixelArray = case getColumn x pixelArray Seq.!? y of
  Nothing -> error $ "Column " <> show y <> " out of bounds"
  Just px -> px

getColumn :: Int -> PixelArray -> Seq Pixel
getColumn y PixelArray {..} = case getPixelArray Seq.!? y of
  Nothing  -> error $ "Column " <> show y <> " out of bounds"
  Just col -> col

getRow :: Int -> PixelArray -> Seq Pixel
getRow x PixelArray {..} = case traverse (Seq.!? x) getPixelArray of
  Nothing  -> error $ "Row " <> show x <> " out of bounds"
  Just row -> row

-- | Parse pixels from a png file with transparency into a 2d array
--
-- @@@
-- pixels <- parsePixelsFromFile "image.png"
-- let
--  firstColumn = pixels ! 0 -- Leftmost column of image.
--  firstRow = fmap (! 0) pixels -- Top row of image.
--  pixel = pixels ! 5 ! 0 -- Pixel at (0,5)
-- @@@
--
parsePixelsFromFile :: FilePath -> IO PixelArray
parsePixelsFromFile filePath = PixelArray <$> do
  surface <- imageSurfaceCreateFromPNG filePath
  stride  <- imageSurfaceGetStride surface
  pixels  <- imageSurfaceGetPixels @Word32 surface

  width   <- imageSurfaceGetWidth surface
  height  <- imageSurfaceGetHeight surface

  let xs = Seq.fromList [0, 1 .. width - 1]
      ys = Seq.fromList [0, 1 .. height - 1]

  for xs $ \x -> do
    for ys $ \y -> Pixel . lowerRGB <$> pixelAt (V2 x y) stride pixels

pixelIndex :: V2 Int -> Int -> Int
pixelIndex (V2 x y) stride = y * (stride `div` 4) + x

pixelAt :: V2 Int -> Int -> SurfaceData Int Word32 -> IO (RGBA Int)
pixelAt v stride surfaceData = do
  word <- readArray surfaceData (pixelIndex v stride)
  pure $ toRGBA word

data RGBA a = RGBA a a a a
  deriving (Show, Eq)

toRGBA :: Word32 -> RGBA Int
toRGBA word = RGBA (c red) (c green) (c blue) (c alpha)
 where
  alpha = (0xFF000000 .&. word) `shiftR` (8 * 3)
  red   = (0x00FF0000 .&. word) `shiftR` (8 * 2)
  green = (0x0000FF00 .&. word) `shiftR` (8 * 1)
  blue  = 0x000000FF .&. word

  c     = fromIntegral . toInteger

lowerRGB :: RGBA Int -> RGB Double
lowerRGB (RGBA r g b _) =
  rgb255 (fromIntegral r) (fromIntegral g) (fromIntegral b)
