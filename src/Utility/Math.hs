module Utility.Math
  ( randomOnHemisphere,
  )
where

import Linear.Metric
import Linear.Quaternion
import Linear.V3
import Linear.Vector
import System.Random.Stateful

randomOnHemisphere :: (StatefulGen g m) => g -> V3 Float -> m (V3 Float)
randomOnHemisphere g normal = do
  s <- uniformRM (0, 1) g
  t <- uniformRM (0, 1) g
  let p = hemisphereConcentricMapping s t
      rot = lookRotation normal (V3 0 0 1)
  return $ rotate rot p

hemisphereConcentricMapping :: Float -> Float -> V3 Float
hemisphereConcentricMapping s t = V3 (u * sqrtR) (v * sqrtR) (1.0 - rSqr)
  where
    a = 2.0 * s - 1.0
    b = 2.0 * t - 1.0
    (theta, phi)
      | a > -b =
          if a > b
            then (pi / 4.0 * (b / a), a)
            else (pi / 4.0 * (2.0 - a / b), b)
      | a < b = (pi / 4.0 * (4.0 + b / a), -a)
      | otherwise = (pi / 4.0 * (6.0 - a / b), -b)
    u = phi * cos theta
    v = phi * sin theta
    rSqr = u * u - v * v
    sqrtR = sqrt (2.0 - rSqr)

lookRotation :: V3 Float -> V3 Float -> Quaternion Float
lookRotation forward up = Quaternion (cos phi) (t ^* sinPhi)
  where
    t = normalize $ cross up forward
    theta = acos $ dot up forward
    phi = theta * 0.5
    sinPhi = sin phi
