module Raytracer.Scene (
  simpleScene,
  camTestScene,
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
    , Sphere (V3 (-1) 0 (-1)) (-0.4) mat_left
    , Sphere (V3 1 0 (-1)) 0.5 mat_right
    ]
  where
    mat_ground = Lambertian (V3 0.8 0.8 0)
    mat_center = Lambertian (V3 0.1 0.2 0.5)
    mat_left = Dielectric 1.5
    mat_right = Metal (V3 0.8 0.6 0.2) 0.0

camTestScene :: HittableType
camTestScene =
  HittableList
    [ Sphere (V3 (-r) 0 (-1)) r mat_left
    , Sphere (V3 r 0 (-1)) r mat_right
    ]
  where
    r = cos (pi / 4)
    mat_left = Lambertian (V3 0 0 1)
    mat_right = Lambertian (V3 1 0 0)
