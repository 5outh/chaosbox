module ChaosBox.Geometry.Path
  ( PathOf(..)
  , Path
  , path
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Geometry.P2
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.AABB
import           ChaosBox.Geometry.Class
import           Control.Lens                   ( (^.) )
import           Data.Foldable                  ( for_ )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Graphics.Rendering.Cairo
                                         hiding ( Path )

newtype PathOf a = PathOf { getPath :: NonEmpty a}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

-- | An open path
type Path = PathOf P2

instance HasV2 a => Affine (PathOf a) where
  transform = defaultTransform

instance HasV2 a => Draw (PathOf a) where
  draw (PathOf (start :| rest)) = do
    newPath
    moveTo (start ^. _V2 . _x) (start ^. _V2 . _y)
    for_ (map (^. _V2) rest) (\(V2 x y) -> lineTo x y)

instance HasV2 a => HasAABB (PathOf a) where
  aabb = boundary . getPath

path :: [a] -> Maybe (PathOf a)
path xs = PathOf <$> NE.nonEmpty xs
