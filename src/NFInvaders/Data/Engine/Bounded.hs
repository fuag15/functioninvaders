-- | Definition of a class to represent any Bounded object in simulations
-- Bounded objects can be tested against for hits by points, lines, and other
-- bounded objects
module NFInvaders.Data.Engine.Bounded (Bounded(..)) where

import NFInvaders.Data.Math.Geometry                 ( Point
                                                     , LineSegment
                                                     , Distance    )


import NFInvaders.Data.Engine.Bounds                 ( Bounds(..)
                                                     , Offset     )

import NFInvaders.Util.Bounded                       (boundedBy)
import NFInvaders.Util.Math                          (mapTuple)
import Data.List                                     (minimumBy)
import Linear.Affine                                 (distanceA)
import Linear.Vector                                 ((^-^))
import Linear.V2                                     (V2(..))
import Data.Ord                                      (comparing)
import Prelude                                hiding (Bounded)

-- | Represents bounded items in the world
-- to instance only a bounds and position function are needed
-- others may be overridden if there are shortcuts to take for a specific object
class Bounded a where
  -- | the bounds of the object and an offset of those bounds coordinates from the objects position
  bounds   :: a -> (Bounds, Offset)

  -- | Position of the object
  position :: a -> Point

  -- | Maximum radius of bounds from objects center
  radius   :: a -> Double
  radius = defaultRadius

  -- | returns a hit or the minimum distance and line segment representing that minimum
  pointCheck :: a
             -> Point                           -- ^ Point to check for collision
             -> (Bool, (Distance, LineSegment)) -- ^ If it hit, if not the minimal distance and segment from the poin to the object
  pointCheck = defaultPointCheck

  -- | returns a hit or the minimum distance and linesegment representing that minimum
  lineSegmentCheck :: a
                   -> LineSegment                     -- ^ Line segment to check for collisionk
                   -> (Bool, (Distance, LineSegment)) -- ^ If it hit, if not the minimal distance and segment from the poin to the object
  lineSegmentCheck = defaultLineSegmentCheck

  -- | returns whether two bounded objects intersect
  boundCheck :: Bounded b
             => a    -- ^ this object
             -> b    -- ^ object to test against
             -> Bool -- ^ result of the check
  boundCheck = defaultBoundCheck

  -- | positions whether two bounded object intersect while temporaroly modifying the other objects position
  -- This allows for Vector path checks where this object remains static
  positionedBoundCheck :: a
                       -> (Bounds, Offset) -- ^ Bounds and Offset of those bounds from origin of the object to test against
                       -> LineSegment      -- ^ Segment representing positions to check for collision at, this object is always at the first point of the line segment
                       -> Bool             -- ^ result of the hit check
  positionedBoundCheck = defaultPositionedBoundCheck


-- | Default implimentation for the classes radius based off of an offset
defaultRadius :: Bounded a
              => a
              -> Double
defaultRadius object = distanceA (V2 0 0) offset
  where
    (_, offset) = bounds object

-- | Default implimentation for classes point hit check based on position and bounds
defaultPointCheck :: Bounded a
                  => a                               -- ^ This bounded object
                  -> Point                           -- ^ point to check for
                  -> (Bool, (Distance, LineSegment)) -- ^ If it hit, if not the minimal distance and segment from the poin to the object
defaultPointCheck object point = (in_bounds, (distance', distance_segment))
  where
    in_bounds        = boundedBy (bounds object) position' point
    distance'        = distanceA position' point
    position'        = position object
    distance_segment = (position', point)

-- | Default implimentation for classes line segment hit check based on pointCheck
defaultLineSegmentCheck :: Bounded a
                        => a                               -- ^ this bounded object
                        -> LineSegment                     -- ^ Line segment to check against
                        -> (Bool, (Distance, LineSegment)) -- ^ If it hit, if not the minimal distance and segment from the poin to the object
defaultLineSegmentCheck object (start, end) = (in_bounds, (distance', distance_segment))
  where
    in_bounds                          = any fst bound_checks
    (_, (distance', distance_segment)) = minimumBy (comparing $ fst . snd) bound_checks
    bound_checks                       = map (pointCheck object) [start, end]


-- | Default implimentation of detailed bound check, first checks radius to see if collision is plausable thendoes a detailed check
-- uses position / radius and then positionedBound check
defaultBoundCheck :: (Bounded a, Bounded b)
                  => a    -- ^ this bounded object
                  -> b    -- ^ bounded object to check for collision
                  -> Bool -- ^ result of test
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

-- | default implimentation of positioned bound check using lineSegmentCheck
defaultPositionedBoundCheck :: Bounded a
                            => a                -- ^ this object
                            -> (Bounds, Offset) -- ^ the bounds and offset of bounds from position of another bounded object
                            -> LineSegment      -- ^ path at which to check collision (this object is always the first point in the line segment)
                            -> Bool             -- ^ result of the check
defaultPositionedBoundCheck object (bounds', offset') (_, hit_position') =
   case bounds' of
     Only   point'        -> hit_position' == point'
     Planar line_segments -> any fst hit_results
       where
         hit_results         = fmap (lineSegmentCheck object) global_space_bounds
         global_space_bounds = fmap (mapTuple (+ world_offset)) line_segments
         world_offset        = V2 0 0 ^-^ (hit_position' - offset')
