module NFInvaders.Util.Math where

import Control.Arrow ((***))
import Control.Monad (join)

inBounds :: Double
            -> Double
            -> Double
            -> Bool
inBounds bound' bound'' x = x <= max' && x >= min'
  where
    max' = max bound' bound''
    min' = min bound' bound''

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)
