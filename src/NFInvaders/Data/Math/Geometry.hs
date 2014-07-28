module NFInvaders.Data.Math.Geometry where

import Linear.V2 (V2(..))

type Point       = V2 Double
type Vector      = V2 Double
type LineSegment = (Point, Point)
type Distance    = Double

data SegmentProjectionResult = BeforeStart | OnSegment | AfterEnd deriving (Eq)

data Path = Only Point | Instantaneous [LineSegment]
