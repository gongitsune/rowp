{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Raytracer.Material where

import Linear (Epsilon (nearZero), V3, normalize)
import Linear.Metric (dot)
import Linear.V3 (V3 (..))
import Linear.Vector ((*^))
import Raytracer.Ray (Ray (..))
import System.Random.Stateful (StatefulGen, UniformRange (uniformRM))
import Utility.Math (randomUnitVector, reflect, refract)

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
      { albedo :: !(V3 Float)
      , fuzz :: !Float
      }
  | Dielectric
      { ir :: !Float -- Index of Refraction
      }

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
scatter (Metal albedo fuzz) ray rec g =
  do
    randomVec <- randomUnitVector g
    let reflected = reflect (normalize ray.direction) rec.normal
        direction = reflected + fuzz *^ randomVec
        scattered = Ray{origin = rec.p, direction}
        attenuation = albedo
    return $
      if direction `dot` rec.normal > 0
        then Just (attenuation, scattered)
        else Nothing
scatter (Dielectric ir) ray rec g = do
  reflectanceRatio <- uniformRM (0, 1) g

  let attenuation = V3 1.0 1.0 1.0
      refractionRatio = if rec.frontFace then 1.0 / ir else ir
      unitDirection = normalize ray.direction
      cosTheta = min (-(unitDirection `dot` rec.normal)) 1.0
      sinTheta = sqrt (1.0 - cosTheta * cosTheta)
      cannotRefract = refractionRatio * sinTheta > 1.0
      direction =
        if cannotRefract || reflectance cosTheta refractionRatio > reflectanceRatio
          then reflect unitDirection rec.normal
          else refract unitDirection rec.normal refractionRatio

  return $ Just (attenuation, Ray{origin = rec.p, direction})
  where
    reflectance :: Float -> Float -> Float
    reflectance cosine refIdx =
      let r0 = (1 - refIdx) / (1 + refIdx)
          r0' = r0 * r0
       in r0' + (1 - r0') * ((1 - cosine) ^ (5 :: Int))
