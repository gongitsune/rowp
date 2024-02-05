{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Raytracer.Camera (
    getRay,
    createCamera,
    CameraCreateInfo (..),
    Camera,
) where

import GHC.Generics (Generic)
import Linear.V3 (V3 (..))
import Linear.Vector ((*^))
import Raytracer.Ray (Ray (..))

data Camera = Camera
    { origin :: !(V3 Float)
    , lowerLeftCorner :: !(V3 Float)
    , horizontal :: !(V3 Float)
    , vertical :: !(V3 Float)
    }
    deriving (Show, Generic)

data CameraCreateInfo = CameraCreateInfo
    { origin :: !(V3 Float)
    , viewportWidth :: !Float
    , viewportHeight :: !Float
    , focalLength :: !Float
    }
    deriving (Show, Generic)

createCamera :: CameraCreateInfo -> Camera
createCamera (CameraCreateInfo{origin, viewportWidth, viewportHeight, focalLength}) =
    Camera
        { origin = origin
        , horizontal = h
        , vertical = v
        , lowerLeftCorner = origin - 0.5 *^ h - 0.5 *^ v - (V3 0 0 focalLength)
        }
  where
    h = V3 viewportWidth 0 0
    v = V3 0 viewportHeight 0

getRay :: Camera -> Float -> Float -> Ray
getRay (Camera{origin, lowerLeftCorner, horizontal, vertical}) u v =
    Ray
        { origin = origin
        , direction = lowerLeftCorner + u *^ horizontal + v *^ vertical - origin
        }
