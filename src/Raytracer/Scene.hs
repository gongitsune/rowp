module Raytracer.Scene (
  simpleScene,
)
where

import Linear.V3 (V3 (..))
import Raytracer.Hittable (HittableType (..))
import Raytracer.Material (Material (..))

simpleScene :: HittableType
simpleScene =
  HittableList
    [ Sphere (V3 0 (-100.5) (-1)) 100 mat_ground
    , Sphere (V3 0 0 (-1)) 0.5 mat_center
    , Sphere (V3 (-1) 0 (-1)) 0.5 mat_left
    , Sphere (V3 1 0 (-1)) 0.5 mat_right
    ]
  where
    mat_ground = Lambertian (V3 0.8 0.8 0)
    mat_center = Lambertian (V3 0.7 0.3 0.3)
    mat_left = Metal (V3 0.8 0.8 0.8) 0.3
    mat_right = Metal (V3 0.8 0.6 0.2) 1
