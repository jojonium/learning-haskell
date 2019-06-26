module Geometry.Cuboid
( volume
, area
) where

volume :: (Floating a) => a -> a -> a -> a
volume l w h = l * w * h

area :: (Floating a) => a -> a -> a -> a
area l w h = (2 * l * w) + (2 * l * h) + (2 * w * h)
