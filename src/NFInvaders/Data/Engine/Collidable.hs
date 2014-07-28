module NFInvaders.Data.Engine.Collidable where

import NFInvaders.Data.Math.Geometry       (Path(..))
import NFInvaders.Data.Engine.Bounded as B (Bounded(..))

class B.Bounded a => Collidable a where
  fixedPositionPathCollision :: B.Bounded b
                                => a
                                -> b
                                -> Path
                                -> Bool
  fixedPositionPathCollision = defaultFixedPositionPathCollision

defaultFixedPositionPathCollision :: (B.Bounded a, B.Bounded b)
                                     => a
                                     -> b
                                     -> Path
                                     -> Bool
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

