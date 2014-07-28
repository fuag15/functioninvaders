module NFInvaders.Data.Engine.Bounds where

import NFInvaders.Data.Math.Geometry ( Point
                                     , Vector
                                     , LineSegment )

data Bounds = Only Point | Planar [LineSegment]

type Offset = Vector
