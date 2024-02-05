{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Raytracer.Ray (
    Ray (..),
    at,
) where

import Linear.V3 (V3)
import Linear.Vector ((*^))

data Ray = Ray {origin :: !(V3 Float), direction :: !(V3 Float)} deriving (Show)

at :: Ray -> Float -> V3 Float
at (Ray o d) t = o + t *^ d
