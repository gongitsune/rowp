module Raytracer.Scene (
  simpleScene,
  randomSphereScene,
)
where

import Linear.Metric (norm)
import Linear.V3 (V3 (..))
import Raytracer.Hittable (HittableType (..))
import Raytracer.Material (Material (..))
import System.Random.Stateful (StatefulGen, UniformRange (uniformRM))
import Utility.Math (randomV3)

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

randomSphereScene :: (StatefulGen g m) => g -> m HittableType
randomSphereScene g =
  HittableList . (\acc -> acc ++ [sphere1, sphere2, sphere3, sphereGround])
    <$> foldl
      ( \acc a ->
          (++) <$> acc <*> foldl (\acc' b -> acc' >>= addRandomHittable g a b) (return []) [(-11) .. 11]
      )
      (return [])
      [(-11) .. 11]
  where
    sphere1 = Sphere (V3 0 1 0) 1.0 (Dielectric 1.5)
    sphere2 = Sphere (V3 (-4) 1 0) 1.0 (Lambertian (V3 0.4 0.2 0.1))
    sphere3 = Sphere (V3 4 1 0) 1.0 (Metal (V3 0.7 0.6 0.5) 0.0)
    sphereGround = Sphere (V3 0 (-1000) 0) 1000.0 (Lambertian (V3 0.5 0.5 0.5))
    addRandomHittable g' a b acc = do
      (chooseMat :: Float) <- uniformRM (0, 1) g'
      center <- V3 . (+ a) . (* 0.9) <$> uniformRM (0, 1) g' <*> return 0.2 <*> ((+ b) . (* 0.9) <$> uniformRM (0, 1) g')

      if norm (center - V3 4 0.2 0) > 0.9
        then
          if chooseMat < 0.8
            then do
              albedo <- (*) <$> randomV3 (0, 1) g' <*> randomV3 (0, 1) g'
              return $ Sphere center 0.2 (Lambertian albedo) : acc
            else
              if chooseMat < 0.95
                then do
                  albedo <- randomV3 (0.5, 1) g'
                  fuzz <- uniformRM (0, 0.5) g'
                  return $ Sphere center 0.2 (Metal albedo fuzz) : acc
                else do
                  return $ Sphere center 0.2 (Dielectric 1.5) : acc
        else
          return acc
