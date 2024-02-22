module Raytracer.Scene (
  simpleScene,
)
where

import Linear.V3 (V3 (..))
import Raytracer.Hittable (HittableType (..))

simpleScene :: HittableType
simpleScene =
  HittableList
    [ Sphere (V3 0 0 (-1)) 0.5
    , Sphere (V3 0 (-100.5) (-1)) 100
    ]
