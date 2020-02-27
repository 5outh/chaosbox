{-# LANGUAGE TypeFamilies #-}
module ChaosBox.Affine
  ( Affine(..)
  , defaultTransform
  , withCairoAffine
  , matrix
  , transformMatrix
  -- * Transformations
  , rotated
  , translated
  , scaled
  , shearedX
  , shearedY
  , sheared
  , reflectedOrigin
  , reflectedX
  , reflectedY
  )
where

import           ChaosBox.Prelude                hiding (scaled)

import           ChaosBox.HasV2
import qualified ChaosBox.Math.Matrix            as Matrix
import           Control.Lens                    ((%~))
import           Graphics.Rendering.Cairo        hiding (transform)
import qualified Graphics.Rendering.Cairo.Matrix as CairoMatrix

newtype Transform2d = Transform2d (M33 Double)

instance Semigroup Transform2d where
  Transform2d a <> Transform2d b = Transform2d $ a !*! b

instance Monoid Transform2d where
  mempty = Transform2d identity

matrix :: M33 Double -> Transform2d
matrix = Transform2d

-- | A class of items that are transformable via linear transformations
class Affine a where
  type Transformed a :: *
  type Transformed a = a
  transform :: Transform2d -> a -> Transformed a

transformMatrix :: Affine a => M33 Double -> a -> Transformed a
transformMatrix = transform . matrix

-- | A useful default 'transform' for 'Functor's over 2D coordinates
--
-- Note: This only works if @Transformed (f a) == f a@
--
defaultTransform :: (Functor f, HasV2 a) => Transform2d -> f a -> f a
defaultTransform (Transform2d m) = fmap (_V2 %~ Matrix.applyMatrix m)

-- | Render something with an 'M33' transformation matrix applied
--
-- @withCairoAffine m render@ resets the 'Matrix' to what it was before
-- @render@ is executed afterwards.
--
withCairoAffine :: Transform2d -> Render () -> Render ()
withCairoAffine (Transform2d (V3 (V3 a b c) (V3 d e f) _)) render = do
  -- Note: Cairo's transformation matrix is column-major and does not contain a
  -- third row.
  let cairoMatrix = CairoMatrix.Matrix a d b e c f
  oldMatrix <- getMatrix
  setMatrix cairoMatrix
  render
  setMatrix oldMatrix

-- Applied transformations

rotated :: Double -> Transform2d
rotated = Transform2d . Matrix.rotation

translated :: V2 Double -> Transform2d
translated = Transform2d . Matrix.translation

scaled :: V2 Double -> Transform2d
scaled = Transform2d . Matrix.scalar

shearedX :: Double -> Transform2d
shearedX = Transform2d . Matrix.shearX

shearedY :: Double -> Transform2d
shearedY = Transform2d . Matrix.shearY

sheared :: V2 Double -> Transform2d
sheared = Transform2d . Matrix.shear

reflectedOrigin :: Transform2d
reflectedOrigin = Transform2d Matrix.reflectOrigin

reflectedX :: Transform2d
reflectedX = Transform2d Matrix.reflectX

reflectedY :: Transform2d
reflectedY = Transform2d Matrix.reflectY
