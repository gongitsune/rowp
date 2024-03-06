{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Raytracer.Camera (
  rayColor,
  createCamera,
  CameraCreateInfo (..),
  Camera,
)
where

import Control.Lens ((^.))
import Linear.Metric (norm, normalize)
import Linear.V3 (V3 (..), cross, _y)
import Linear.Vector (lerp, (*^), (^/))
import Raytracer.Hittable (HittableType, hit)
import Raytracer.Material (HitRecord (mat), scatter)
import Raytracer.Ray (Ray (..))
import System.Random.Stateful (StatefulGen, uniformRM)
import Utility.Interval (Interval (..))
import Utility.Math (Radians (..), rad2Float)

data Camera = Camera
  { origin :: !(V3 Float)
  , pixel00Loc :: !(V3 Float)
  , pixelDeltaU :: !(V3 Float)
  , pixelDeltaV :: !(V3 Float)
  , spp :: !Int
  , maxDepth :: !Int
  }

data CameraCreateInfo = CameraCreateInfo
  { aspectRatio :: !Float
  , imageHeight :: !Int
  , imageWidth :: !Int
  , spp :: !Int
  , maxDepth :: !Int
  , vfov :: !Radians
  , lookFrom :: !(V3 Float)
  , lookAt :: !(V3 Float)
  , vup :: !(V3 Float)
  }

createCamera :: CameraCreateInfo -> Camera
createCamera info =
  Camera
    { origin = info.lookFrom
    , pixel00Loc
    , pixelDeltaU
    , pixelDeltaV
    , spp = info.spp
    , maxDepth = info.maxDepth
    }
  where
    w = normalize $ info.lookFrom - info.lookAt
    u = normalize $ info.vup `cross` w
    v = w `cross` u
    h = rad2Float $ tan (info.vfov / 2.0)
    focalLength = norm (info.lookFrom - info.lookAt)
    viewportHeight = 2.0 * h * focalLength
    viewportWidth = info.aspectRatio * viewportHeight
    viewportU = viewportWidth *^ u
    viewportV = viewportHeight *^ (-v)
    viewportUL = info.lookFrom - (viewportU / 2) - (viewportV / 2) - (focalLength *^ w)
    pixelDeltaU = viewportU ^/ fromIntegral info.imageWidth
    pixelDeltaV = viewportV ^/ fromIntegral info.imageHeight
    pixel00Loc = viewportUL + 0.5 * (pixelDeltaU + pixelDeltaV)

getRay :: (StatefulGen g m) => Camera -> Int -> Int -> g -> m Ray
getRay c x y g = do
  sample <- pixelSampleSquare c g
  let pixelCenter = c.pixel00Loc + (fromIntegral x *^ c.pixelDeltaU) + (fromIntegral y *^ c.pixelDeltaV)
      pixelSample = pixelCenter + sample
  return
    Ray
      { origin = c.origin
      , direction = pixelSample - c.origin
      }

pixelSampleSquare :: (StatefulGen g m) => Camera -> g -> m (V3 Float)
pixelSampleSquare c g = do
  u <- uniformRM (0, 1) g
  v <- uniformRM (0, 1) g
  return $ (u *^ c.pixelDeltaU) + (v *^ c.pixelDeltaV)

rayColor :: (StatefulGen g m) => Camera -> (Int, Int) -> HittableType -> g -> m (V3 Float)
rayColor c (x, y) world g = color ^/ fromIntegral c.spp
  where
    color = go 0 c.spp g
    go acc 0 _ = return acc
    go acc n g' = do
      r <- getRay c x y g'
      sample <- samplePixel g' r c.maxDepth world
      go (acc + sample) (n - 1) g'

samplePixel :: (StatefulGen g m) => g -> Ray -> Int -> HittableType -> m (V3 Float)
samplePixel _ _ 0 _ = return $ V3 0 0 0
samplePixel g ray depth world
  | Just rec <- hit world ray (Interval 0.001 (1 / 0)) = do
      scatterResult <- scatter rec.mat ray rec g
      case scatterResult of
        Just (attenuation, scattered) -> do
          sample <- samplePixel g scattered (depth - 1) world
          return $ attenuation * sample
        Nothing -> return $ V3 0 0 0
  | otherwise =
      return $
        let unitDirection = normalize ray.direction
            t = 0.5 * (unitDirection ^. _y + 1.0)
         in lerp t (V3 0.5 0.7 1.0) (V3 1 1 1)
