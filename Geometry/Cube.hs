module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: (Floating a) => a -> a
volume l = Cuboid.volume l l l

area :: (Floating a) => a -> a
area l = Cuboid.volume l l l
