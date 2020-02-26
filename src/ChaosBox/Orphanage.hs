{-# OPTIONS_GHC -fno-warn-orphans #-}
module ChaosBox.Orphanage
  ()
where

import           Data.Random (Distribution, Distribution (..), Normal,
                              Normal (..))
import           Linear.V2

-- These orphan instances do not leak to the end-user, since they're only used
-- in wrapper functions.

instance Distribution Normal a => Distribution Normal (V2 a) where
  rvarT StdNormal = V2 <$> rvarT StdNormal <*> rvarT StdNormal
  rvarT (Normal (V2 mx my) (V2 sx sy)) =
    V2 <$> rvarT (Normal mx sx) <*> rvarT (Normal my sy)
