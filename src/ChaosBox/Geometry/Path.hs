module ChaosBox.Geometry.Path
  ( Path(Path)
  , getPath
  , path
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           Data.Foldable            (for_)
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty       as NE
import           Graphics.Rendering.Cairo hiding (Path)

-- | An open path
data Path = Path { _path :: NonEmpty (V2 Double), pathMatrix :: M33 Double }
  deriving (Show, Eq, Ord)

instance Affine Path where
  matrixLens wrap (Path p m) = fmap (Path p) (wrap m)

instance Draw Path where
  draw (Path (V2 startX startY :| rest) m) = withCairoAffine m $ do
    newPath
    moveTo startX startY
    for_ rest (\(V2 x y) -> lineTo x y)

-- NB. this operation and those like it are useful but should not be a part of
-- the public API.
bakePath :: Path -> Path
bakePath = withReset $ \p -> p { _path = fmap (applyAffine p) (_path p) }

getPath :: Path -> NonEmpty (V2 Double)
getPath = _path . bakePath

path :: [V2 Double] -> Maybe Path
path xs = Path <$> NE.nonEmpty xs <*> pure identity
