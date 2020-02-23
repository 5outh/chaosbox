module ChaosBox.Affine
  ( Affine(..)
  , resetMatrix
  , withReset
  , applyAffine
  , withCairoAffine
  , transformed
  )
where

import           ChaosBox.Prelude

import qualified ChaosBox.Math.Matrix            as Matrix
import           Control.Lens
import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Matrix as CairoMatrix

-- | A class of items that are transformable via linear transformations
class Affine a where
  matrixLens :: Lens' a (M33 Double)

-- | Reset a transformation matrix to 'identity'
resetMatrix :: Affine a => a -> a
resetMatrix = set matrixLens identity

-- | Apply a function and reset the transformation matrix afterwards.
--
-- This is commonly used for "baking", where the transformation matrix gets
-- applied to the underlying data type directly in haskell values so we don't
-- need to hold onto the matrix anymore.
--
withReset :: (Affine a, Affine b) => (a -> b) -> a -> b
withReset f a = resetMatrix (f a)

-- | Get a V2 transformation from the 'Affine' transformation
applyAffine :: Affine a => a -> V2 Double -> V2 Double
applyAffine = Matrix.apply . view matrixLens

-- | Render something with an 'M33' transformation matrix applied
--
-- @withCairoAffine m render@ resets the 'Matrix' to what it was before
-- @render@ is executed afterwards.
--
withCairoAffine :: M33 Double -> Render () -> Render ()
withCairoAffine (V3 (V3 a b c) (V3 d e f) _) render = do
  -- Note: Cairo's transformation matrix is column-major and does not contain a
  -- third row.
  let cairoMatrix = CairoMatrix.Matrix a d b e c f
  oldMatrix <- getMatrix
  setMatrix cairoMatrix
  render
  setMatrix oldMatrix

-- Applied transformations

transformed :: Affine a => M33 Double -> a -> a
transformed m a = a & matrixLens %~ (!*! m)

-- rotation
-- translation
-- scalar
-- shearX
-- shearY
-- shear
-- reflectOrigin
-- reflectX
-- reflectY
