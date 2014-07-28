module NFInvaders.Util.Bounded where

import NFInvaders.Data.Math.Geometry       ( SegmentProjectionResult(..)
                                           , Point )

import NFInvaders.Data.Engine.Bounds       ( Bounds(..)
                                           , Offset )

import NFInvaders.Util.Math                (mapTuple)
import Control.Lens                        ((^.))
import Linear.V2                           (V2(..), _x)
import Linear.Vector                       ((^-^))
import NFInvaders.Util.Geometry            (linearProjection)
import Data.Maybe                          (isJust)

boundedBy :: (Bounds, Offset)
          -> Point
          -> Point
          -> Bool
boundedBy (bounds, offset) origin point@(V2 px _) =
  case bounds of
    Only   point'        -> point == point'
    Planar line_segments -> odd $ length left_hits
      where
        left_hits       = [ projection | Just (hit_result, projection) <- projected_hits
                                      , hit_result == OnSegment
                                      , projection^._x < px ]
        projected_hits  = filter isJust $ map (`linearProjection` point) global_segments
        global_segments = fmap (mapTuple (+ global_offset)) line_segments
        global_offset   = V2 0 0 ^-^ (origin - offset)
