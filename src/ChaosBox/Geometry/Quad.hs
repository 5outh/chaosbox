module ChaosBox.Geometry.Quad
  ( Quad(..)
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Math.Matrix           ( applyMatrix )
import           ChaosBox.Geometry.Polygon      ( polygon )
import           ChaosBox.Affine
import           ChaosBox.Draw
import           Data.Foldable                  ( for_ )

data Quad = Quad
  { quadA      :: V2 Double
  , quadB      :: V2 Double
  , quadC      :: V2 Double
  , quadD      :: V2 Double
  }

instance Affine Quad where
  transform m q = q
    { quadA = applyMatrix m (quadA q)
    , quadB = applyMatrix m (quadB q)
    , quadC = applyMatrix m (quadC q)
    , quadD = applyMatrix m (quadD q)
    }

instance Draw Quad where
  draw Quad {..} = for_ (polygon [quadA, quadB, quadC, quadD])
    draw
