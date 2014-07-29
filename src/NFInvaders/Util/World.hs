-- | simple utility functions for the world
module NFInvaders.Util.World where

import NFInvaders.Data.Engine.World   as W (World(..))
import NFInvaders.Data.Engine.Bounded as B (Bounded(..))

-- | returns whether a bounded object is in the world or not
inWorld :: B.Bounded a
        => World -- ^ World to check against
        -> a     -- ^ Bounded object to test
        -> Bool  -- ^ result of the check
inWorld world object =
  case bounds object of
    Only   point    -> fst $ pointCheck world point
    Planar segments -> any (fst . lineCheck world) segments
