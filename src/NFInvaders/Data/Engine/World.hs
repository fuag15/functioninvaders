{-# LANGUAGE TemplateHaskell #-}
module NFInvaders.Data.World where

import NFInvaders.Data.Engine.Bounds       ( Bounds(..)
                                           , Offset )

import NFInvaders.Data.Math.Geometry       ( Point
                                           , Distance
                                           , LineSegment )

import NFInvaders.Util.Geometry            (boundingBoxPoint)

import Control.Lens                        ( makeLenses
                                           , (^.) )

import Linear.V2                           (V2(..))
import Linear.Affine                       (distanceA)
import NFInvaders.Data.Engine.Bounded as B (Bounded(..))

data World = World { _width  :: Double
                   , _height :: Double }

$(makeLenses ''World)

instance B.Bounded World where
  bounds           = worldBounds
  position         = worldPosition
  pointCheck       = worldPointCheck

worldBounds :: World -> (Bounds, Offset)
worldBounds world = (bounds', offset)
  where
    bounds'        = Planar [width_segment, height_segment]
    width_segment  = (V2 0 0, V2 (world^.width) 0              )
    height_segment = (V2 0 0, V2 0              (world^.height))
    offset         = V2 (world^.width / 2) $ world^.height / 2


worldPosition :: World -> Point
worldPosition world = V2 (world^.width / 2) (world^.height / 2)

worldPointCheck :: World
                -> Point
                -> (Bool, (Distance, LineSegment))
worldPointCheck world point = (in_world, (distance', distance_segment))
  where
    in_world         = boundingBoxPoint (V2 0 0) (V2 (world^.width) (world^.height)) point
    distance_segment = (point, worldPosition world)
    distance'        = uncurry distanceA distance_segment
