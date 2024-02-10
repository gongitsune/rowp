{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Raytracer.Camera
  ( getRay,
    rayColor,
    createCamera,
    CameraCreateInfo (..),
    Camera,
  )
where

import Control.Lens ((^.))
import Control.Monad (replicateM)
import Linear.Metric (normalize)
import Linear.V3 (V3 (..), _y)
import Linear.Vector (lerp, (*^), (^/))
import Raytracer.Hittable (HitRecord (..), HittableType, hit)
import Raytracer.Ray (Ray (..))
import System.Random.Stateful (StatefulGen, uniformRM)
import Utility.Interval (Interval (..))

data Camera = Camera
  { origin :: !(V3 Float),
    pixel00Loc :: !(V3 Float),
    pixelDeltaU :: !(V3 Float),
    pixelDeltaV :: !(V3 Float)
  }
  deriving (Show)

data CameraCreateInfo = CameraCreateInfo
  { origin :: !(V3 Float),
    aspectRatio :: !Float,
    imageHeight :: !Int,
    imageWidth :: !Int,
    viewportHeight :: !Float,
    focalLength :: !Float
  }
  deriving (Show)

createCamera :: CameraCreateInfo -> Camera
createCamera info =
  Camera
    { origin = info.origin,
      pixel00Loc,
      pixelDeltaU,
      pixelDeltaV
    }
  where
    viewportWidth = info.aspectRatio * info.viewportHeight
    viewportU = V3 viewportWidth 0 0
    viewportV = V3 0 info.viewportHeight 0
    viewportUL = info.origin - (viewportU / 2) - (viewportV / 2) - V3 0 0 info.focalLength
    pixelDeltaU = viewportU ^/ fromIntegral info.imageWidth
    pixelDeltaV = viewportV ^/ fromIntegral info.imageHeight
    pixel00Loc = viewportUL + 0.5 * (pixelDeltaU + pixelDeltaV)

getRay :: (StatefulGen g m) => Camera -> g -> Int -> Int -> m Ray
getRay c g x y = do
  sample <- pixelSampleSquare c g
  let pixelCenter = c.pixel00Loc + (fromIntegral x *^ c.pixelDeltaU) + (fromIntegral y *^ c.pixelDeltaV)
      pixelSample = pixelCenter + sample
  return
    Ray
      { origin = c.origin,
        direction = pixelSample - c.origin
      }

pixelSampleSquare :: (StatefulGen g m) => Camera -> g -> m (V3 Float)
pixelSampleSquare c g = do
  u <- uniformRM (0, 1) g
  v <- uniformRM (0, 1) g
  return $ (u *^ c.pixelDeltaU) + (v *^ c.pixelDeltaV)

rayColor :: (StatefulGen g m) => Camera -> g -> (Int, Int) -> HittableType -> m (V3 Float)
rayColor c g (x, y) world = do
  rays <- replicateM 100 $ getRay c g x y
  let color = foldl (\acc ray -> acc + rayColor' ray) (V3 0 0 0) rays
  return $ color ^/ 100
  where
    rayColor' ray
      | Just rec <- hit world ray (Interval 0 (1 / 0)) =
          0.5 * (rec.normal + 1)
      | otherwise =
          let t = 0.5 * (unitDirection ^. _y + 1.0)
           in lerp t (V3 0.5 0.7 1.0) (V3 1 1 1)
      where
        unitDirection = normalize ray.direction
