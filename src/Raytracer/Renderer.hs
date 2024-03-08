{-# LANGUAGE DuplicateRecordFields #-}

module Raytracer.Renderer (render) where

import Codec.Picture (generateImage, savePngImage)
import Codec.Picture.Types (DynamicImage (..), PixelRGB8 (..))
import Control.Lens ((^.))
import GHC.Float.RealFracMethods (floorFloatInt)
import Linear.V3 (V3 (..), _x, _y, _z)
import Raytracer.Camera (Camera, CameraCreateInfo (..), createCamera, rayColor)
import Raytracer.Hittable (HittableType (..))
import Raytracer.Scene (randomSphereScene)
import System.Environment (getArgs)
import System.Random.Stateful (mkStdGen, runStateGen_)
import Utility.Math (deg2rad)

aspectRatio :: Float
aspectRatio = 16.0 / 9.0

imgWidth :: Int
imgWidth = 1200

imgHeight :: Int
imgHeight = floorFloatInt $ fromIntegral imgWidth / aspectRatio

camera :: Camera
camera =
  createCamera
    CameraCreateInfo
      { aspectRatio
      , imageHeight = imgHeight
      , imageWidth = imgWidth
      , spp = 50
      , maxDepth = 50
      , vfov = deg2rad 20
      , lookFrom = V3 13 2 3
      , lookAt = V3 0 0 0
      , vup = V3 0 1 0
      , defocusAngle = deg2rad 0.6
      , focusDistance = 10.0
      }

render :: IO ()
render = do
  [path] <- getArgs

  let world = runStateGen_ (mkStdGen 0) randomSphereScene
      img = generateImage (pixelFn world) imgWidth imgHeight
  savePngImage path (ImageRGB8 img)

pixelFn :: HittableType -> Int -> Int -> PixelRGB8
pixelFn world x y = toPixel $ runStateGen_ g $ rayColor camera (x, y) world
  where
    g = mkStdGen $ x * imgHeight + y + 20

toPixel :: V3 Float -> PixelRGB8
toPixel c =
  let gamma = sqrt c
      r = fromIntegral $ floorFloatInt (gamma ^. _x * 255.999)
      g = fromIntegral $ floorFloatInt (gamma ^. _y * 255.999)
      b = fromIntegral $ floorFloatInt (gamma ^. _z * 255.999)
   in PixelRGB8 r g b
