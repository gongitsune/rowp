{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Raytracer.Renderer (render) where

import Codec.Picture (DynamicImage, generateImage, savePngImage)
import Codec.Picture.Types (DynamicImage (..), PixelRGB8 (..))
import Control.Lens ((^.))
import GHC.Float.RealFracMethods (floorFloatInt)
import Linear.Metric (dot, normalize)
import Linear.V3 (V3 (..), _x, _y, _z)
import Linear.Vector (lerp)
import Raytracer.Camera (Camera, CameraCreateInfo (..), createCamera, getRay)
import Raytracer.Ray (Ray (..))
import Raytracer.Ray qualified as Ray
import System.Environment (getArgs)

aspectRatio :: Float
aspectRatio = 16.0 / 9.0
imgWidth :: Int
imgWidth = 384
imgHeight :: Int
imgHeight = floorFloatInt $ (fromIntegral imgWidth) / aspectRatio
camera :: Camera
camera =
    createCamera
        CameraCreateInfo
            { origin = V3 0 0 0
            , viewportHeight = 2.0
            , viewportWidth = aspectRatio * 2.0
            , focalLength = 1.0
            }

render :: IO ()
render = do
    [path] <- getArgs
    savePngImage path genImg

genImg :: DynamicImage
genImg = ImageRGB8 (generateImage pixelFn imgWidth imgHeight)

hitSphere :: V3 Float -> Float -> Ray -> Float
hitSphere center radius ray =
    let oc = ray.origin - center
        a = ray.direction `dot` ray.direction
        b = 2.0 * oc `dot` ray.direction
        c = oc `dot` oc - radius * radius
        discriminant = b * b - 4 * a * c
     in if discriminant < 0 then -1 else (-b - sqrt discriminant) / (2.0 * a)

rayColor :: Ray -> V3 Float
rayColor ray
    | t > 0 =
        let normal = normalize $ Ray.at ray t - (V3 0 0 (-1))
         in 0.5 * (normal + 1)
    | otherwise =
        let t' = 0.5 * (unitDirection ^. _y + 1.0)
         in lerp t' (V3 0.5 0.7 1.0) (V3 1 1 1)
  where
    unitDirection = normalize ray.direction
    t = hitSphere (V3 0 0 (-1)) 0.5 ray

pixelFn :: Int -> Int -> PixelRGB8
pixelFn x y =
    let u = fromIntegral x / (fromIntegral imgWidth - 1)
        v = fromIntegral (imgHeight - y - 1) / (fromIntegral imgHeight - 1)
     in toPixel $ rayColor $ getRay camera u v

toPixel :: V3 Float -> PixelRGB8
toPixel c =
    let ir = fromIntegral $ floorFloatInt (c ^. _x * 255.999)
        ig = fromIntegral $ floorFloatInt (c ^. _y * 255.999)
        ib = fromIntegral $ floorFloatInt (c ^. _z * 255.999)
     in PixelRGB8 ir ig ib