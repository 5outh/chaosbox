-- | Open paths
module ChaosBox.Geometry.Path
  ( PathOf(..)
  , Path
  , pattern Path
  , getPath
  )
where

import           ChaosBox.Prelude

import           ChaosBox.AABB
import           ChaosBox.Draw
import           ChaosBox.Geometry.Angle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           Control.Lens            ((^.))
import           Data.Foldable           (for_)
import           Data.List.NonEmpty      (NonEmpty (..))
import           GI.Cairo.Render         hiding (Path)

newtype PathOf a = PathOf { getPathOf :: NonEmpty a }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

-- | An open path
type Path = PathOf P2

pattern Path :: NonEmpty P2 -> Path
pattern Path { getPath } = PathOf getPath
{-# COMPLETE Path #-}

instance HasP2 a => Draw (PathOf a) where
  draw (PathOf (start :| rest)) = do
    newPath
    moveTo (start ^. _V2 . _x) (start ^. _V2 . _y)
    for_ (map (^. _V2) rest) (\(V2 x y) -> lineTo x y)

instance HasP2 a => HasAABB (PathOf a) where
  aabb = boundary . getPathOf

translatePath :: P2 -> Path -> Path
translatePath p2 = fmap (translateP2 p2)

scalePath :: Double -> Path -> Path
scalePath amount = fmap (scaleP2 amount)

scalePathAround :: P2 -> Double -> Path -> Path
scalePathAround center amount = fmap (scaleP2Around center amount)

rotatePath :: Angle -> Path -> Path
rotatePath theta = fmap (rotateP2 theta)

rotatePathAround :: P2 -> Angle -> Path -> Path
rotatePathAround center theta = fmap (rotateP2Around center theta)
