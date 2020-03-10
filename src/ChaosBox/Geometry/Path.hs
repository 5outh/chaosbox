module ChaosBox.Geometry.Path
  ( PathOf(..)
  , Path
  , pattern Path
  , pathOf
  , path
  )
where

import           ChaosBox.Prelude

import           ChaosBox.AABB
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           Control.Lens            ((^.))
import           Data.Foldable           (for_)
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NE
import           GI.Cairo.Render         hiding (Path)

newtype PathOf a = PathOf { getPath :: NonEmpty a}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

-- | An open path
type Path = PathOf P2

pattern Path :: NonEmpty P2 -> Path
pattern Path a = PathOf a
{-# COMPLETE Path #-}

instance HasP2 a => Affine (PathOf a) where
  transform = defaultTransform

instance HasP2 a => Draw (PathOf a) where
  draw (PathOf (start :| rest)) = do
    newPath
    moveTo (start ^. _V2 . _x) (start ^. _V2 . _y)
    for_ (map (^. _V2) rest) (\(V2 x y) -> lineTo x y)

instance HasP2 a => HasAABB (PathOf a) where
  aabb = boundary . getPath

pathOf :: [a] -> Maybe (PathOf a)
pathOf xs = PathOf <$> NE.nonEmpty xs

path :: [P2] -> Maybe Path
path = pathOf @P2
