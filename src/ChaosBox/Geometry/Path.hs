module ChaosBox.Geometry.Path
  ( Path(..)
  , path
  , bakePath
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           Data.Foldable            (for_)
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Graphics.Rendering.Cairo hiding (Path)

-- | An open path
data Path = Path { getPath :: NonEmpty (V2 Double), pathMatrix :: M33 Double }
  deriving (Show, Eq, Ord)

instance Affine Path where
  matrixLens wrap (Path p m) = fmap (Path p) (wrap m)

instance Draw Path where
  draw (Path (V2 startX startY :| rest) m) = withCairoAffine m $ do
    newPath
    moveTo startX startY
    for_ rest (\(V2 x y) -> lineTo x y)

path :: [V2 Double] -> Maybe Path
path xs = Path <$> NE.nonEmpty xs <*> pure identity

bakePath :: Path -> Path
bakePath = withReset $ \p -> p { getPath = fmap (applyAffine p) (getPath p) }

-- TODO: implement this interface
-- transformed -> deferred
-- transform   -> multiply by transform and bake
-- bake        -> apply transformation matrix
