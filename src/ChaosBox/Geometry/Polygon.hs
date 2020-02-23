module ChaosBox.Geometry.Polygon
  ( Polygon(..)
  , polygon
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Math.Matrix     (applyMatrix)
import           Data.Foldable            (for_)
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty       as NE
import           Graphics.Rendering.Cairo hiding (Path)

-- | A closed path
newtype Polygon = Polygon { getPolygon :: NonEmpty (V2 Double) }
  deriving (Show, Eq, Ord)

instance Affine Polygon where
  transform m = Polygon . fmap (applyMatrix m) . getPolygon

instance Draw Polygon where
  draw (Polygon (V2 startX startY :| rest)) = do
    newPath
    moveTo startX startY
    for_ rest (\(V2 x y) -> lineTo x y)
    closePath

polygon :: [V2 Double] -> Maybe Polygon
polygon xs = Polygon <$> NE.nonEmpty xs
