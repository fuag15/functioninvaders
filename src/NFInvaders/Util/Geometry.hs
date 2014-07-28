module NFInvaders.Util.Geometry where

import NFInvaders.Data.Math.Geometry ( SegmentProjectionResult(..)
                                     , Point
                                     , LineSegment
                                     , Distance )

import Data.List                     ( minimumBy
                                     , union )

import Linear.V2                     (V2(..))
import NFInvaders.Util.Math          (inBounds)
import Linear.Affine                 (distanceA)
import Data.Tuple                    (swap)
import Data.Ord                      (comparing)

intersects :: LineSegment
              -> LineSegment
              -> Bool
intersects (V2 x1 y1, V2 x2 y2) (V2 x3 y3, V2 x4 y4)
  | all (== 0) [denom, numer_a, numer_b]                   = True  -- Coincident
  | denom == 0                                             = False -- Parallel
  | all (inBounds 0 1) $ fmap (/ denom) [numer_a, numer_b] = True  -- Intersect
  | otherwise                                              = False
  where
    denom   = (y4 - y3)*(x2 - x1) - (x4 - x3)*(y2 - y1)
    numer_a = (x4 - x3)*(y1 - y3) - (y4 - y3)*(x1 - x3)
    numer_b = (x2 - x1)*(y1 - y3) - (y2 - y1)*(x1 - x3)

minimalPathLineSegmentLineSegment :: LineSegment
                                     -> LineSegment
                                     -> (Distance, LineSegment)
minimalPathLineSegmentLineSegment line@(start, end) line'@(start', end')
   | path `elem` first_biased = (distance, path)
   | otherwise                = (distance, swap path)
   where
     (distance, path) = minimumBy (comparing fst) merged
     merged            = [ (distanceA p p', l) | l@(p, p') <- first_biased `union` second_biased ]
     first_biased      = [ minimalPathLineSegmentPoint line  point | point <- [start', end'] ]
     second_biased     = [ minimalPathLineSegmentPoint line' point | point <- [start , end ] ]

minimalPathLineSegmentPoint :: LineSegment
                               -> Point
                               -> LineSegment
minimalPathLineSegmentPoint line@(start, end) point =
  case linearProjection line point of
    Just (BeforeStart, _         ) -> (start     , point)
    Just (AfterEnd   , _         ) -> (end       , point)
    Just (OnSegment  , projection) -> (projection, point)
    _                              -> (start     , point)

projectLineSegmentPoint :: LineSegment
                           -> Point
                           -> Maybe Point
projectLineSegmentPoint line point =
  case linearProjection line point of
    Just (_, projection) -> Just projection
    _                    -> Nothing

linearProjection :: LineSegment
                    -> Point
                    -> Maybe (SegmentProjectionResult, Point)
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

boundingBoxPoint :: Point
                 -> Point
                 -> Point
                 -> Bool
boundingBoxPoint (V2 lx ly) (V2 ux uy) (V2 px py) =
  inBounds lx ux px && inBounds ly uy py
