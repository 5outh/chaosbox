module ChaosBox.Geometry.Circle
  ( Circle(..)
  , circle
  , point
  )
where

import           ChaosBox.Prelude        hiding ( point )

import           ChaosBox.Affine
import           ChaosBox.Draw
import           Graphics.Rendering.Cairo

-- | A circle with radius 'circleRadius' centered at 'circleCenter'
data Circle = Circle { circleCenter :: V2 Double, circleRadius :: Double, circleMatrix :: M33 Double }
  deriving (Show, Eq, Ord)

instance Affine Circle where
  matrixLens wrap (Circle c r m) = fmap (Circle c r) (wrap m)

instance Draw Circle where
  draw Circle {..} = withCairoAffine circleMatrix $ do
    let V2 x y = circleCenter
    moveTo (x + circleRadius) y
    arc x y circleRadius 0 (2 * pi)

circle :: V2 Double -> Double -> Circle
circle v r = Circle v r identity

-- | A circle with diameter 1
point :: V2 Double -> Circle
point center = Circle center 0.5 identity
