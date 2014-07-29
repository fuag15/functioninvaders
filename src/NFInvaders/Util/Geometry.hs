-- | Nitty gritty geometric bound and collision check utilities
module NFInvaders.Util.Geometry where

import NFInvaders.Data.Math.Geometry ( SegmentProjectionResult(..)
                                     , Point
                                     , LineSegment
                                     , Distance                    )

import Data.List                     ( minimumBy
                                     , union     )

import Linear.V2                     (V2(..))
import NFInvaders.Util.Math          (inBounds)
import Linear.Affine                 (distanceA)
import Data.Tuple                    (swap)
import Data.Ord                      (comparing)

{-|
  checks two line segments for intersection

  this function uses the following bit of algebra to determine intersection

  first, convert the line into the form Ax + By = C
  given a line segment (x, y) (x', y') this yields

  > A = y' - y
  > B = x  - x'
  > C = Ax + By

  given two lines A x + B y = C
  and             A'x + B'y = C'

  then solve for the intersection point, x and y being equal
  do this the standard way by multiplying both equations by the opposites b term
  and then the a term to eliminate x and y respectively and then taking the difference of the two
  for the x term this would look like

  > B'Ax + B'By = B'C
  > BA'x + B'By = BC'
  > AB'x - A'Bx = B'C - BC'

  dividing both sides by the determinant, AB' - A'B
  yields the final equation for x

  > scale_x = (B'C - BC') / (AB' - A'B)

  which gives this for the actual x point of intersection

  > x_intersect = x + scale_x * (x' - x)

  where x, x' are the x coordinates of the first line segment

  Since this computation only cares if they intersect stop checking at this stage
  if the resulting scale is between 0 and 1 the intersection lies in the segment

  it is a similar story for y

  if the determinant is 0 the lines are parallel
  if the determinant and the numerator of both functions are zero the lines are coincident
-}
intersects :: LineSegment -- ^ first segment
           -> LineSegment -- ^ second segment
           -> Bool
intersects (V2 x1 y1, V2 x2 y2) (V2 x3 y3, V2 x4 y4)
  | all (== 0) [determinant, numer_x, numer_y]                   = True  -- Coincident
  | denom == 0                                                   = False -- Parallel
  | all (inBounds 0 1) $ fmap (/ determinant) [numer_x, numer_y] = True  -- Intersect
  | otherwise                                                    = False
  where
    determinant = (y4 - y3)*(x2 - x1) - (x4 - x3)*(y2 - y1)
    numer_x     = (x4 - x3)*(y1 - y3) - (y4 - y3)*(x1 - x3)
    numer_y     = (x2 - x1)*(y1 - y3) - (y2 - y1)*(x1 - x3)

-- | finds the minimal path between two line segments
-- the returned line segment has its first point on the first line
-- and its end point on the second supplied line
minimalPathLineSegmentLineSegment :: LineSegment             -- ^ first segment to test
                                  -> LineSegment             -- ^ second segment
                                  -> (Distance, LineSegment) -- ^ distance of minimal path, line segment representing path
minimalPathLineSegmentLineSegment line@(start, end) line'@(start', end')
   | path `elem` first_biased = (distance, path)
   | otherwise                = (distance, swap path)
   where
     (distance, path) = minimumBy (comparing fst) merged
     merged            = [ (distanceA p p', l) | l@(p, p') <- first_biased `union` second_biased ]
     first_biased      = [ minimalPathLineSegmentPoint line  point | point <- [start', end'] ]
     second_biased     = [ minimalPathLineSegmentPoint line' point | point <- [start , end ] ]

{-|
  Returns the minimal path in the form of a line segment form a line segment to point
  the path is always such that the first point in the returned segment rests on the line segment
  and the second point in the line segment is the point

  if the result of the linearProjection was Nothing then the line is a point
  the minimal path in this case is a line from the point to the point
-}
minimalPathLineSegmentPoint :: LineSegment -- ^ line segment to check against
                            -> Point       -- ^ point to check
                            -> LineSegment -- ^ shortest line segment from line segment to point
minimalPathLineSegmentPoint line@(start, end) point =
  case linearProjection line point of
    Just (BeforeStart, _         ) -> (start     , point)
    Just (AfterEnd   , _         ) -> (end       , point)
    Just (OnSegment  , projection) -> (projection, point)
    _                              -> (start     , point)

{-|
  projects a point onto a line and returns either
  the distance from the point to the line along with whether
  the projection resulted in a hit or above or below

  If the denom was 0 then the line segment is actually a point
  in this case if the point matches the start of the segment return a hit
  if the point is not return Nothing as a hit or above / blow does not make sense

  calculations are done according to this:

  Given a line segment: start = (x, y), end = (x', y')
  and a point:          point = (px, py)

  get the line defined by the line segment:

  > line = start + scalar (end - start)

  the closest path to the point from the line defined by the segment will be tangent to the line
  this can be represented by

  > (point - line) dot (end - start) = 0

  substituting the line equation in and solving for scalar forms

  > (point - start - scalar (end - start)) dot (end - start) = 0
  > scalar = ((px - x)(x' - x) + (py - y)(y' - y)) / (length (end - start) ^2)

  substituting that into the equation for the line yields equations for the projections coordinates

  > projection_x = x + scalar (x' - x)
  > projection_y = y + scalar (y' - y)

  since the point is not needed unless it falls on the segment the scale is first filtered for
  a number bounded between [0, 1]
-}
linearProjection :: LineSegment                            -- ^ Line Segment to project onto
                 -> Point                                  -- ^ point to project
                 -> Maybe (SegmentProjectionResult, Point) -- ^ The type of projection (above, below, on), and projected point or Nothing
linearProjection (start@(V2 sx sy), end@(V2 ex ey)) point@(V2 px py)
  | denom == 0 && start == point = Just (OnSegment, point)
  | denom == 0                   = Nothing
  | otherwise                    = Just (segment_check, projection)
  where
    denom         = distanceA start end ** 2
    numer         = (px - sx)*(ex - sx) + (py - sy)* (ey - sy)
    scale         = numer / denom
    projection    = start + V2 scale scale * (end - start)
    segment_check
      | scale < 0 = BeforeStart
      | scale > 1 = AfterEnd
      | otherwise = OnSegment

-- | Simplified point check for a bounding box
boundingBoxPoint :: Point -- ^ lower left hand corner of box
                 -> Point -- ^ upper right hand corner of box
                 -> Point -- ^ point to test for inclusion in box
                 -> Bool  -- ^ result of test
boundingBoxPoint (V2 lx ly) (V2 ux uy) (V2 px py) =
  inBounds lx ux px && inBounds ly uy py
