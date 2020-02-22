module ChaosBox.Math.Matrix
  (
  -- * Affine transformations
    rotation
  , translation
  , scalar
  , shearX
  , shearY
  , shear
  , reflectOrigin
  , reflectX
  , reflectY
  -- * Vector Operations
  , transform
  , apply
  -- * Re-exports
  , identity
  )
where

import           Linear.Matrix hiding (translation)
import           Linear.V2
import           Linear.V3

-- type M33 a = V3 (V3 a)
--

-- brittany --exact-print-only

affine :: a -> a -> a
       -> a -> a -> a
       -> a -> a -> a
       -> M33 a
affine
  a b c
  d e f
  g h i
  = V3
    (V3 a b c)
    (V3 d e f)
    (V3 g h i)

rotation :: Double -> M33 Double
rotation t = affine
  (cos t)    (sin t) 0
  (-(sin t)) (cos t) 0
  0          0       1

translation :: V2 Double -> M33 Double
translation (V2 x y) = affine
  1 0 x
  0 1 y
  0 0 1

scalar :: V2 Double -> M33 Double
scalar (V2 w h) = affine
  w 0 0
  0 h 0
  0 0 1

shearX :: Double -> M33 Double
shearX t = affine
  1 (tan t) 0
  0 1       0
  0 0       1

shearY :: Double -> M33 Double
shearY t = affine
  1       0 0
  (tan t) 1 0
  0 0     1

shear :: V2 Double -> M33 Double
shear (V2 x y) = shearX x * shearY y

reflectOrigin :: M33 Double
reflectOrigin = affine
  (-1) 0    0
  0    (-1) 0
  0    0    1

reflectX :: M33 Double
reflectX = affine
  1 0    0
  0 (-1) 0
  0 0    1

reflectY :: M33 Double
reflectY = affine
  (-1) 0 0
  0    1 0
  0    0 1

transform :: V2 Double -> M33 Double -> V2 Double
transform v@(V2 x y) m = V2 x0 y0
 where
  v0 = V3 x y 0
  V3 x0 y0 _ = m !* v0

apply :: M33 Double -> V2 Double -> V2 Double
apply = flip transform
