-- | Data definition for the bounds and Offset data
module NFInvaders.Data.Engine.Bounds where

import NFInvaders.Data.Math.Geometry ( Point
                                     , Vector
                                     , LineSegment )

-- | Engine currently supports either point or planar bounds
data Bounds = Only Point | Planar [LineSegment]

-- | an affine vector space delta
type Offset = Vector
