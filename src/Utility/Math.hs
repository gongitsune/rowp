module Utility.Math (
  randomOnHemisphere,
  randomUnitVector,
  reflect,
  refract,
  Radians (..),
  Degrees (..),
  rad2deg,
  deg2rad,
  rad2Float,
  deg2Float,
  randomInUnitDisk,
  randomV3,
)
where

import Control.Lens ((^.))
import Linear.Metric (Metric (dot), normalize)
import Linear.Quaternion (Quaternion (..), rotate)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..), cross, _x, _y)
import Linear.Vector ((*^), (^*))
import System.Random.Stateful (
  StatefulGen,
  UniformRange (uniformRM),
 )

newtype Radians = Radians Float deriving (Eq, Ord, Num, Fractional, Floating)
newtype Degrees = Degrees Float deriving (Eq, Ord, Num, Fractional, Floating)

instance Show Radians where
  show (Radians r) = show r ++ " [rad]"

instance Show Degrees where
  show (Degrees d) = show d ++ " [deg]"

rad2deg :: Radians -> Degrees
rad2deg (Radians r) = Degrees (57.29577951308232 * r)

deg2rad :: Degrees -> Radians
deg2rad (Degrees d) = Radians (1.7453292519943295e-2 * d)

rad2Float :: Radians -> Float
rad2Float (Radians r) = r

deg2Float :: Degrees -> Float
deg2Float (Degrees d) = d

randomV3 :: (UniformRange a, StatefulGen g m) => (a, a) -> g -> m (V3 a)
randomV3 range g = V3 <$> uniformRM range g <*> uniformRM range g <*> uniformRM range g

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
      | b == 0 = (0, -b)
      | otherwise = (pi / 4.0 * (6.0 - a / b), -b)
    u = phi * cos theta
    v = phi * sin theta
    rSqr = u * u + v * v
    sqrtR = sqrt (2.0 - rSqr)

randomUnitVector :: (StatefulGen g m) => g -> m (V3 Float)
randomUnitVector g = do
  s <- uniformRM (0, 1) g
  t <- uniformRM (0, 1) g
  return $ sphereConcentricMapping s t

sphereConcentricMapping :: Float -> Float -> V3 Float
sphereConcentricMapping s t = V3 x y z
  where
    u = 2.0 * s - 1.0
    v = 2.0 * t - 1.0
    (r, phi, scale)
      | u >= 0 && v >= 0 =
          let phi' = (pi / 4) * ((v - u) / r + 1)
           in if u + v <= 1
                then (u + v, phi', 1)
                else (2 - u - v, phi', -1)
      | u <= 0 && v >= 0 =
          let phi' = (pi / 4) * (1 - (v + u) / r) + pi / 2
           in if -u + v <= 1
                then (-u + v, phi', 1)
                else (2 + u - v, phi', -1)
      | u <= 0 && v <= 0 =
          let phi' = (pi / 4) * ((-v + u) / r + 1) + pi
           in if -u - v <= 1
                then (-u - v, phi', 1)
                else (2 + u + v, phi', -1)
      | otherwise =
          let phi' = (pi / 4) * (1 - (-v - u) / r) + 3 * pi / 2
           in if u - v <= 1
                then (u - v, phi', 1)
                else (2 - u + v, phi', -1)

    x = r * sqrt (2.0 - r * r) * cos phi
    y = r * sqrt (2.0 - r * r) * sin phi
    z = (1 - r * r) * scale

randomInUnitDisk :: (StatefulGen g m) => g -> m (V3 Float)
randomInUnitDisk g = do
  s <- uniformRM (0, 1) g
  t <- uniformRM (0, 1) g
  let p = circleConcentricMapping s t
  return $ V3 (p ^. _x) (p ^. _y) 0

circleConcentricMapping :: Float -> Float -> V2 Float
circleConcentricMapping s t = V2 (r * cos theta) (r * sin theta)
  where
    u = 2.0 * s - 1.0
    v = 2.0 * t - 1.0
    (r, theta)
      | u == 0 && v == 0 = (0, 0)
      | abs u > abs v = (u, pi / 4 * (v / u))
      | otherwise = (v, pi / 2 - pi / 4 * (u / v))

lookRotation :: V3 Float -> V3 Float -> Quaternion Float
lookRotation forward up = Quaternion (cos phi) (t ^* sinPhi)
  where
    t = normalize $ cross up forward
    theta = acos $ dot up forward
    phi = theta * 0.5
    sinPhi = sin phi

reflect :: V3 Float -> V3 Float -> V3 Float
reflect v n = v - 2 * (v `dot` n) *^ n

refract :: V3 Float -> V3 Float -> Float -> V3 Float
refract uv n etaiOverEtat =
  let cosTheta = min (-(uv `dot` n)) 1.0
      rOutPerp = etaiOverEtat *^ (uv + cosTheta *^ n)
      rOutParallel = -(sqrt (abs (1.0 - dot rOutPerp rOutPerp)) *^ n)
   in rOutPerp + rOutParallel
