-- | simple math function utilities
module NFInvaders.Util.Math where

import NFInvaders.Data.Math.Geometry (Point)
import Linear.V2                     (V2(..))
import Control.Arrow                 ((***))
import Control.Monad                 (join)

-- | tests if a number is boudned by two others
inBounds :: Double -- ^ one edge of the bounds
         -> Double -- ^ the other edge of the bounds
         -> Double -- ^ the number to chec
         -> Bool   -- ^ result of hte bound chec
inBounds bound' bound'' x = x <= max' && x >= min'
  where
    max' = max bound' bound''
    min' = min bound' bound''

-- | clamps a Vector to the given Box
-- used to keep braveDefender in the 'right' spot
clampPointToBox :: (Point, Point) -- ^ two corners of the box
                -> Point          -- ^ current state of position
                -> Point          -- ^ clamped poition result
clampPointToBox (V2 bx by, V2 bx' by') (V2 x y) = V2 clamped_x clamped_y
   where
     clamped_x = clampToBounds bx bx' x
     clamped_y = clampToBounds by by' y

-- | clamps a value inbetween two bounds
clampToBounds :: Double -- ^ one edge of the bounds
              -> Double -- ^ other edge of the bounds
              -> Double -- ^ current value
              -> Double -- ^ clamped value
clampToBounds bound' bound'' = bounded_above . bounded_below
  where
    bounded_below x = max x min'
    bounded_above x = min x max'
    max'            = max bound' bound''
    min'            = min bound' bound''

-- | definition of fmap for a tuple
-- this actually uses some arrow logic
-- we are splitting the tuple up and sending
-- the function to both splits then joining the result
-- back into a tuple
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)
