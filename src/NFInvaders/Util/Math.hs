-- | simple math function utilities
module NFInvaders.Util.Math where

import Control.Arrow ((***))
import Control.Monad (join)

-- | tests if a number is boudned by two others
inBounds :: Double -- ^ one edge of the bounds
         -> Double -- ^ the other edge of the bounds
         -> Double -- ^ the number to chec
         -> Bool   -- ^ result of hte bound chec
inBounds bound' bound'' x = x <= max' && x >= min'
  where
    max' = max bound' bound''
    min' = min bound' bound''

-- | definition of fmap for a tuple
-- this actually uses some arrow logic
-- we are splitting the tuple up and sending
-- the function to both splits then joining the result
-- back into a tuple
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)
