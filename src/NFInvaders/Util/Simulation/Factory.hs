-- | wire utilities that generate lists of wires
module NFInvaders.Util.Simulation.Factory where

import Control.Wire.Core ( Wire
                         , mkSF_ )

import Data.Monoid       (Monoid)

-- | A wire to take inputs and generate wires from them
wireFactory :: (Monad m, Monoid s)
            => (x -> Wire s e m a b)           -- ^ takes input and generates a wire from it
            -> Wire s e m [x] [Wire s e m a b] -- ^ wire that takes list of inputs, outputs list of wires
wireFactory arrow_generator = mkSF_ $ \generator_inputs ->
  fmap arrow_generator generator_inputs
