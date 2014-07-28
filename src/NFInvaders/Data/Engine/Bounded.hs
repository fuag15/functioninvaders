module NFInvaders.Data.Engine.Bounded (Bounded(..)) where

import NFInvaders.Data.Math.Geometry                 ( Point
                                                     , LineSegment
                                                     , Distance )


import NFInvaders.Data.Engine.Bounds                 ( Bounds(..)
                                                     , Offset )

import NFInvaders.Util.Bounded                       (boundedBy)
import NFInvaders.Util.Math                          (mapTuple)
import Data.List                                     (minimumBy)
import Linear.Affine                                 (distanceA)
import Linear.Vector                                 ((^-^))
import Linear.V2                                     (V2(..))
import Data.Ord                                      (comparing)
import Prelude                                hiding (Bounded)

class Bounded a where
  bounds   :: a -> (Bounds, Offset)
  position :: a -> Point

  radius   :: a -> Double
  radius = defaultRadius

  pointCheck :: a
             -> Point
             -> (Bool, (Distance, LineSegment))
  pointCheck = defaultPointCheck

  lineSegmentCheck :: a
                   -> LineSegment
                   -> (Bool, (Distance, LineSegment))
  lineSegmentCheck = defaultLineSegmentCheck

  boundCheck :: Bounded b
             => a
             -> b
             -> Bool
  boundCheck = defaultBoundCheck

  positionedBoundCheck :: a
                          -> (Bounds, Offset)
                          -> LineSegment
                          -> Bool
  positionedBoundCheck = defaultPositionedBoundCheck


defaultRadius :: Bounded a
                 => a
                 -> Double
defaultRadius object = distanceA (V2 0 0) offset
  where
    (_, offset) = bounds object

defaultPointCheck :: Bounded a
                     => a
                     -> Point
                     -> (Bool, (Distance, LineSegment))
defaultPointCheck object point = (in_bounds, (distance', distance_segment))
  where
    in_bounds        = boundedBy (bounds object) position' point
    distance'        = distanceA position' point
    position'        = position object
    distance_segment = (position', point)

defaultLineSegmentCheck :: Bounded a
                        => a
                        -> LineSegment
                        -> (Bool, (Distance, LineSegment))
defaultLineSegmentCheck object (start, end) = (in_bounds, (distance', distance_segment))
  where
    in_bounds                          = any fst bound_checks
    (_, (distance', distance_segment)) = minimumBy (comparing $ fst . snd) bound_checks
    bound_checks                       = map (pointCheck object) [start, end]


defaultBoundCheck :: (Bounded a, Bounded b)
                     => a
                     -> b
                     -> Bool
defaultBoundCheck object object' =
  case pointCheck object position' of
      (True , _)                    -> True
      (False, (distance, hit_line))
        | distance > possible_hit   -> False
        | otherwise                 -> detailed_check hit_line
  where
    position'      = position object'
    radius'        = radius   object'
    possible_hit   = radius' + radius object
    detailed_check = positionedBoundCheck object (bounds object')

defaultPositionedBoundCheck :: Bounded a
                             => a
                             -> (Bounds, Offset)
                             -> LineSegment
                             -> Bool
defaultPositionedBoundCheck object (bounds', offset') (_, hit_position') =
   case bounds' of
     Only   point'        -> hit_position' == point'
     Planar line_segments -> any fst hit_results
       where
         hit_results         = fmap (lineSegmentCheck object) global_space_bounds
         global_space_bounds = fmap (mapTuple (+ world_offset)) line_segments
         world_offset        = V2 0 0 ^-^ (hit_position' - offset')
