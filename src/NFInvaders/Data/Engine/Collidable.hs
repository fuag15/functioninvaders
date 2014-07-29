-- | collidable class and default implimentation
module NFInvaders.Data.Engine.Collidable where

import NFInvaders.Data.Math.Geometry       (Path(..))
import NFInvaders.Data.Engine.Bounded as B (Bounded(..))

-- | represents a collidable object in the world
-- must be an instance of bounded
-- you do not need to impliment anythign to instance this class
-- you may want to re-imliment fixedPositionPathCollision
class B.Bounded a => Collidable a where
  -- | checks for a collision with another bounded object along a path
  -- this check assumes the path is instantaneousely traversed by the other object (ignores time)
  -- this check is against this collidable held at a fixed position
  fixedPositionPathCollision :: B.Bounded b
                             => a    -- ^ this collidable
                             -> b    -- ^ bounded object to be tested against
                             -> Path -- ^ bounded objects path
                             -> Bool -- ^ result of collision check
  fixedPositionPathCollision = defaultFixedPositionPathCollision

-- | Default implimentation of fixedPositionPathCollision
-- check the line segments of the path for possible collsions
-- returns true if any position along the path collided
-- note that this function is a bit sloppy in the case of the path being empty
-- this should be resolved by stronger data declarations
defaultFixedPositionPathCollision :: (B.Bounded a, B.Bounded b)
                                  => a    -- ^ this collidable
                                  -> b    -- ^ bounded object to be tested against
                                  -> Path -- ^ path of bounded object
                                  -> Bool -- ^ result of collision check
defaultFixedPositionPathCollision object collider collider_path =
  case collider_path of
    Only          _                    -> B.boundCheck object collider
    Instantaneous line_segments
      | any fst simple_collisions      -> True
      | not $ null possible_collisions -> or detailed_collisions
      where
        simple_collisions                   = fmap (lineSegmentCheck object) line_segments
        possible_collisions                 = [ collision | collision@(_, (distance, _)) <- simple_collisions
                                                          , distance <= hit_chance ]
        hit_chance                          = radius object + radius collider
        detailed_collisions                 = fmap positioned_check possible_collisions
        positioned_check (_, (_, hit_line)) = B.positionedBoundCheck object (bounds collider) hit_line
    _                                  -> B.boundCheck object collider

