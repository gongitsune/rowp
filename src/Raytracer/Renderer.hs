{-# LANGUAGE DuplicateRecordFields #-}

module Raytracer.Renderer (render) where

import Codec.Picture (generateImage, savePngImage)
import Codec.Picture.Types (DynamicImage (..), PixelRGB8 (..))
import Control.Lens ((^.))
import GHC.Float.RealFracMethods (floorFloatInt)
import Linear.V3 (V3 (..), _x, _y, _z)
import Raytracer.Camera (Camera, CameraCreateInfo (..), createCamera, rayColor)
import Raytracer.Hittable (HittableType (..))
import Raytracer.Scene (simpleScene)
import System.Environment (getArgs)
import System.Random.Stateful (mkStdGen)

aspectRatio :: Float
aspectRatio = 16.0 / 9.0

imgWidth :: Int
imgWidth = 400

imgHeight :: Int
imgHeight = floorFloatInt $ fromIntegral imgWidth / aspectRatio

camera :: Camera
camera =
  createCamera
    CameraCreateInfo
      { origin = V3 0 0 0
      , aspectRatio
      , imageHeight = imgHeight
      , imageWidth = imgWidth
      , viewportHeight = 2.0
      , focalLength = 1.0
      , spp = 50
      }

world :: HittableType
world = simpleScene

render :: IO ()
render = do
  [path] <- getArgs

  let img = generateImage pixelFn imgWidth imgHeight
  savePngImage path (ImageRGB8 img)

pixelFn :: Int -> Int -> PixelRGB8
pixelFn x y = toPixel $ rayColor g camera (x, imgHeight - y - 1) world
  where
    g = mkStdGen $ x * imgHeight + y + 20

toPixel :: V3 Float -> PixelRGB8
toPixel c =
  let ir = fromIntegral $ floorFloatInt (c ^. _x * 255.999)
      ig = fromIntegral $ floorFloatInt (c ^. _y * 255.999)
      ib = fromIntegral $ floorFloatInt (c ^. _z * 255.999)
   in PixelRGB8 ir ig ib
