{-# LANGUAGE TemplateHaskell #-}

-- | Data representation of an orthographic world to simulate the game in
module NFInvaders.Data.Engine.World where

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

-- | a 2d world is fully defined by its width and height
data World = World { _width  :: Double
                   , _height :: Double }

-- | utillity lenses for accessing / modifying data
$(makeLenses ''World)

-- | A world is bounded
instance B.Bounded World where
  bounds           = worldBounds
  position         = worldPosition
  pointCheck       = worldPointCheck

-- | For the world its bounds is its width and height
-- the offset is simply the vector form origin to the center of it
worldBounds :: World -> (Bounds, Offset)
worldBounds world = (bounds', offset)
  where
    bounds'        = Planar [width_segment, height_segment]
    width_segment  = (V2 0 0, V2 (world^.width) 0              )
    height_segment = (V2 0 0, V2 0              (world^.height))
    offset         = V2 (world ^. width / 2) $ world ^. height / 2


-- | The position of the center of the world is stagnant.
-- its the center of the world
worldPosition :: World -> Point
worldPosition world = V2 (world ^. width / 2) (world ^. height / 2)

-- | Point checks for the world are simplified because it is a simple box check
worldPointCheck :: World
                -> Point
                -> (Bool, (Distance, LineSegment))
worldPointCheck world point = (in_world, (distance', distance_segment))
  where
    in_world         = boundingBoxPoint (V2 0 0) (V2 (world ^. width) (world ^. height)) point
    distance_segment = (point, worldPosition world)
    distance'        = uncurry distanceA distance_segment
