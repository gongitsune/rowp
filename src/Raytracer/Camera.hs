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
import Linear.Metric (normalize)
import Linear.V3 (V3 (..), _y)
import Linear.Vector (lerp, (*^), (^/))
import Raytracer.Hittable (HitRecord (..), HittableType, hit)
import Raytracer.Ray (Ray (..))
import System.Random.Stateful (RandomGen, StatefulGen, runStateGen_, uniformRM)
import Utility.Interval (Interval (..))
import Utility.Math (randomOnHemisphere)

data Camera = Camera
  { origin :: !(V3 Float)
  , pixel00Loc :: !(V3 Float)
  , pixelDeltaU :: !(V3 Float)
  , pixelDeltaV :: !(V3 Float)
  , spp :: !Int
  }
  deriving (Show)

data CameraCreateInfo = CameraCreateInfo
  { origin :: !(V3 Float)
  , aspectRatio :: !Float
  , imageHeight :: !Int
  , imageWidth :: !Int
  , viewportHeight :: !Float
  , focalLength :: !Float
  , spp :: !Int
  }
  deriving (Show)

createCamera :: CameraCreateInfo -> Camera
createCamera info =
  Camera
    { origin = info.origin
    , pixel00Loc
    , pixelDeltaU
    , pixelDeltaV
    , spp = info.spp
    }
  where
    viewportWidth = info.aspectRatio * info.viewportHeight
    viewportU = V3 viewportWidth 0 0
    viewportV = V3 0 info.viewportHeight 0
    viewportUL = info.origin - (viewportU / 2) - (viewportV / 2) - V3 0 0 info.focalLength
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

rayColor :: (RandomGen g) => g -> Camera -> (Int, Int) -> HittableType -> V3 Float
rayColor g c (x, y) world = color ^/ fromIntegral c.spp
  where
    color = runStateGen_ g (go 0 c.spp)
    go acc 0 _ = return acc
    go acc n g' = do
      r <- getRay c x y g'
      sample <- samplePixel g' r world
      go (acc + sample) (n - 1) g'

samplePixel :: (StatefulGen g m) => g -> Ray -> HittableType -> m (V3 Float)
samplePixel g ray world
  | Just rec <- hit world ray (Interval 0.001 (1 / 0)) = do
      direction <- randomOnHemisphere g rec.normal
      let newRay = Ray rec.p direction
      0.5 *^ samplePixel g newRay world
  | otherwise =
      return $
        let unitDirection = normalize ray.direction
            t = 0.5 * (unitDirection ^. _y + 1.0)
         in lerp t (V3 0.5 0.7 1.0) (V3 1 1 1)
