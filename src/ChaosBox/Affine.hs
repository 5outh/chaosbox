{-# LANGUAGE TypeFamilies #-}
module ChaosBox.Affine
  (
  -- * Data Types
    Transform2d(..)
  , defaultTransform
  , withCairoAffine
  , matrix
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
  -- * Combinators
  , around
  )
where

import           ChaosBox.Prelude        hiding (scaled)

import           ChaosBox.Geometry.Class (HasP2 (..))
import           ChaosBox.Geometry.P2
import qualified ChaosBox.Math.Matrix    as Matrix
import           Control.Lens            ((%~))
import           GI.Cairo.Render         hiding (transform)
import qualified GI.Cairo.Render.Matrix  as CairoMatrix

-- | 2d transformation represented as a 3x3 Matrix
--
-- In 'Transform2d's 'Semigroup' and 'Monoid' instances, 'mempty' is the
-- 'identity' matrix and '<>' is matrix multiplication. This means
-- 'Transform2d's created in this module can be combined with regular
-- 'semigroup' append. For example, the following 'Transform2d' represents
-- translating 5 units in the Y direction, then rotating in the Y direction by
-- @pi@ radians around the point @(10, 0)@:
--
-- @translated (V2 0 5) <> around (V2 10 0) (rotated pi)@
--
newtype Transform2d = Transform2d { getTransform2d :: M33 Double }

instance Semigroup Transform2d where
  Transform2d a <> Transform2d b = Transform2d $ a !*! b

instance Monoid Transform2d where
  mempty = Transform2d identity

-- | Create a 'Transform2d' from a 3x3 matrix ('M33')
matrix :: M33 Double -> Transform2d
matrix = Transform2d

-- | A useful default 'transform' for 'Functor's over 2D coordinates
--
-- Note: This only works if @Transformed (f a) == f a@
--
defaultTransform :: (Functor f, HasP2 a) => Transform2d -> f a -> f a
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

-- | Rotation matrix by @n@ radians counter-clockwise.
rotated :: Double -> Transform2d
rotated = Transform2d . Matrix.rotation

-- | Translation matrix by an offset vector
translated :: P2 -> Transform2d
translated = Transform2d . Matrix.translation

-- | Scalar matrix by a scaling vector.
--
-- @scaled (P2 3 2)@ represents a 3x scale in the x direction and a 3x scale in
-- the y direction.
scaled :: P2 -> Transform2d
scaled = Transform2d . Matrix.scalar

-- | Shear matrix in the x direction
shearedX :: Double -> Transform2d
shearedX = Transform2d . Matrix.shearX

-- | Shear matrix in the y direction
shearedY :: Double -> Transform2d
shearedY = Transform2d . Matrix.shearY

-- | Shear matrix in both directions
--
-- @shered (P2 x y) = shearedX x <> shearedY y@
--
sheared :: P2 -> Transform2d
sheared = Transform2d . Matrix.shear

-- | Reflection about the origin (0,0)
reflectedOrigin :: Transform2d
reflectedOrigin = Transform2d Matrix.reflectOrigin

-- | Reflection across the x axis
reflectedX :: Transform2d
reflectedX = Transform2d Matrix.reflectX

-- | Reflection across the y axis
reflectedY :: Transform2d
reflectedY = Transform2d Matrix.reflectY

-- | A 'Transform2d' run after shifting to some point
--
-- For example (assume @center@ is the center of some shape):
--
-- @transform (around center (rotated (pi/2))) shape@
--
-- will rotate @shape@ by @pi/2@ radians about its @center@.
--
around :: P2 -> Transform2d -> Transform2d
around v (Transform2d m) = Transform2d (Matrix.aroundMatrix v m)
