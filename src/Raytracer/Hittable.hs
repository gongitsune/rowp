{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Raytracer.Hittable (
    HitRecord (..),
    HittableType (..),
    hit,
) where

import Linear.Metric (dot)
import Linear.V3 (V3 (..))
import Linear.Vector ((^/))
import Raytracer.Ray (Ray (..))
import Raytracer.Ray qualified as Ray

data HitRecord = HitRecord
    { p :: !(V3 Float)
    , normal :: !(V3 Float)
    , frontFace :: !Bool
    , t :: !Float
    }

data HittableType
    = HittableList ![HittableType]
    | Sphere
        { center :: !(V3 Float)
        , radius :: !Float
        }

hit :: HittableType -> Ray -> Float -> Float -> Maybe HitRecord
hit (HittableList hittables) r tMin tMax = go Nothing tMax hittables
  where
    go (Just rec) _ [] = Just rec
    go Nothing _ [] = Nothing
    go rec closest (h : hs)
        | Just hitRec <- hit h r tMin closest = go (Just hitRec) hitRec.t hs
        | otherwise = go rec closest hs
hit (Sphere center radius) r tMin tMax
    | discriminant < 0 = Nothing
    | Just t <- closerT =
        let p = Ray.at r t
            (normal, frontFace) = faceNormal r ((p - center) ^/ radius)
         in Just HitRecord{p, normal, frontFace, t}
    | otherwise = Nothing
  where
    oc = r.origin - center
    a = r.direction `dot` r.direction
    halfB = oc `dot` r.direction
    c = oc `dot` oc - radius * radius
    discriminant = halfB * halfB - a * c

    closerT
        | t1 < tMax && tMin < t1 = Just t1
        | t2 < tMax && tMin < t2 = Just t2
        | otherwise = Nothing
      where
        root = sqrt discriminant
        t1 = (-halfB - root) / a
        t2 = (-halfB + root) / a

-- | Utility
faceNormal :: Ray -> V3 Float -> (V3 Float, Bool)
faceNormal r outwardNormal
    | frontFace = (outwardNormal, True)
    | otherwise = (-outwardNormal, False)
  where
    frontFace = r.direction `dot` outwardNormal < 0
