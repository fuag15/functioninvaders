module NFInvaders.Util.World where

import NFInvaders.Data.Engine.World   as W (World(..))
import NFInvaders.Data.Engine.Bounded as B (Bounded(..))

inWorld :: B.Bounded a
        => World
        -> a
        -> Bool
inWorld world object =
  case bounds object of
    Only   point    -> fst $ pointCheck world point
    Planar segments -> any (fst . lineCheck world) segments
