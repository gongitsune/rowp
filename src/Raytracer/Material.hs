{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Raytracer.Material where

import Linear (Epsilon (nearZero), V3, normalize)
import Raytracer.Ray (Ray (..))
import System.Random.Stateful (StatefulGen)
import Utility.Math (randomUnitVector, reflect)

data HitRecord = HitRecord
  { p :: !(V3 Float)
  , normal :: !(V3 Float)
  , mat :: !Material
  , frontFace :: !Bool
  , t :: !Float
  }

data Material
  = Lambertian
      {albedo :: !(V3 Float)}
  | Metal
      {albedo :: !(V3 Float)}

scatter :: (StatefulGen g m) => Material -> Ray -> HitRecord -> g -> m (Maybe (V3 Float, Ray))
scatter (Lambertian albedo) _ rec g =
  do
    randomVec <- randomUnitVector g
    let scatterDirection = rec.normal + randomVec
        scattered =
          Ray
            { origin = rec.p
            , direction =
                if nearZero scatterDirection
                  then rec.normal
                  else scatterDirection
            }
        attenuation = albedo
    return $ Just (attenuation, scattered)
scatter (Metal albedo) ray rec _ =
  do
    let reflected = reflect (normalize ray.direction) rec.normal
        scattered = Ray{origin = rec.p, direction = reflected}
        attenuation = albedo
    return $ Just (attenuation, scattered)
