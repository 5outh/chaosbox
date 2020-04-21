-- | Open paths
module ChaosBox.Geometry.Path
  ( PathOf(..)
  , Path
  , pattern Path
  , getPath
  , translatePath
  , scalePath
  , scalePathAround
  , rotatePath
  , rotatePathAround
  )
where

import           ChaosBox.Prelude

import           ChaosBox.AABB
import           ChaosBox.Draw
import           ChaosBox.Geometry.Angle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Transform
import           Control.Lens                ((^.))
import           Data.Foldable               (for_)
import           Data.List.NonEmpty          (NonEmpty (..))
import           GI.Cairo.Render             hiding (Path)

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

translatePath :: HasP2 a => P2 -> PathOf a -> PathOf a
translatePath = translatePoints

scalePath :: HasP2 a => P2 -> PathOf a -> PathOf a
scalePath = scalePoints

scalePathAround :: HasP2 a => P2 -> P2 -> PathOf a -> PathOf a
scalePathAround = scaleAroundPoints

rotatePath :: HasP2 a => Angle -> PathOf a -> PathOf a
rotatePath = rotatePoints

rotatePathAround :: HasP2 a => P2 -> Angle -> PathOf a -> PathOf a
rotatePathAround = rotateAroundPoints
