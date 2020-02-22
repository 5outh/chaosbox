module ChaosBox.Geometry.Quad
  ( Quad(..)
  , quad
  , bakeQuad
  )
where

import           ChaosBox.Prelude

import           Control.Lens                   ( set )
import           ChaosBox.Geometry.Polygon      ( polygon )
import           ChaosBox.Affine
import           ChaosBox.Draw
import           Data.Foldable                  ( for_ )

data Quad = Quad
  { quadA      :: V2 Double
  , quadB      :: V2 Double
  , quadC      :: V2 Double
  , quadD      :: V2 Double
  , quadMatrix :: M33 Double
  }

quad :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> Quad
quad a b c d = Quad a b c d identity

instance Affine Quad where
  matrixLens wrap (Quad a b c d m) = fmap (Quad a b c d) (wrap m)

instance Draw Quad where
  draw Quad {..} = for_ (polygon [quadA, quadB, quadC, quadD])
    $ draw . set matrixLens quadMatrix

bakeQuad :: Quad -> Quad
bakeQuad q@(Quad a b c d _) = Quad (applyAffine q a)
                                   (applyAffine q b)
                                   (applyAffine q c)
                                   (applyAffine q d)
                                   identity
