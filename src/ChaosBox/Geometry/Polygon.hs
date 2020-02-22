module ChaosBox.Geometry.Polygon
  ( Polygon(..)
  , polygon
  , bakePolygon
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           Data.Foldable            (for_)
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty       as NE
import           Graphics.Rendering.Cairo hiding (Path)

-- | A closed path
data Polygon = Polygon { getPolygon :: NonEmpty (V2 Double), polygonMatrix :: M33 Double}
  deriving (Show, Eq, Ord)

instance Affine Polygon where
  matrixLens wrap (Polygon p m) = fmap (Polygon p) (wrap m)

instance Draw Polygon where
  draw (Polygon (V2 startX startY :| rest) m) = withCairoAffine m $ do
    newPath
    moveTo startX startY
    for_ rest (\(V2 x y) -> lineTo x y)
    closePath


polygon :: [V2 Double] -> Maybe Polygon
polygon xs = Polygon <$> NE.nonEmpty xs <*> pure identity

bakePolygon :: Polygon -> Polygon
bakePolygon =
  withReset $ \p -> p { getPolygon = fmap (applyAffine p) (getPolygon p) }
