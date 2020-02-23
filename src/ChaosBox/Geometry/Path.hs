module ChaosBox.Geometry.Path
  ( Path(..)
  , path
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Math.Matrix     (applyMatrix)
import           Data.Foldable            (for_)
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Graphics.Rendering.Cairo hiding (Path)

-- | An open path
newtype Path = Path { getPath :: NonEmpty (V2 Double) }
  deriving (Show, Eq, Ord)

instance Affine Path where
  transform m = Path . fmap (applyMatrix m) . getPath

instance Draw Path where
  draw (Path (V2 startX startY :| rest)) = do
    newPath
    moveTo startX startY
    for_ rest (\(V2 x y) -> lineTo x y)

path :: [V2 Double] -> Maybe Path
path xs = Path <$> NE.nonEmpty xs
