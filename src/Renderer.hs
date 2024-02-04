module Renderer (render) where

import System.Environment (getArgs)
import Codec.Picture (DynamicImage, generateImage, savePngImage)
import Codec.Picture.Types (PixelRGB8(..), DynamicImage(..))

render :: IO ()
render = do
  [path] <- getArgs
  savePngImage path genImg

genImg :: DynamicImage
genImg = ImageRGB8 (generateImage pixelFn 1200 1200)

pixelFn :: Int -> Int -> PixelRGB8
pixelFn x y =
  let r = fromIntegral x / 1200.0
      g = fromIntegral y / 1200.0
      b = 0.25
  in toPixel r g b

toPixel :: Float -> Float -> Float -> PixelRGB8
toPixel r g b =
  let ir = fromIntegral $ (floor (r * 255.999) :: Int)
      ig = fromIntegral $ (floor (g * 255.999) :: Int)
      ib = fromIntegral $ (floor (b * 255.999) :: Int)
  in PixelRGB8 ir ig ib
