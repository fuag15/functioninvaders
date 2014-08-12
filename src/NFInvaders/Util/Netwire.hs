-- | netwire remappings
module NFInvaders.Util.Netwire where

import Control.Wire.Core ( Wire
                         , stepWire )

import Data.Monoid       (Monoid)

-- | A version of stepwire with the arguments flipped around for convienence
stepWire' :: (Monad m, Monoid s)
          => s                              -- ^ time delta
          -> Either e a                     -- ^ input
          -> Wire s e m a b                 -- ^ wire to step
          -> m (Either e b, Wire s e m a b) -- ^ output paired with new wire state
stepWire' dt input wire = stepWire wire dt input

-- | Take a list of results of stepWire and filter out inhibited wires
removeInhibited :: (Monad m, Monoid s)
                => [(Either e b, Wire s e m a b)] -- ^ stepWire results to filter
                -> [(b, Wire s e m a b)]          -- ^ filtered results
removeInhibited stepped_wires = [(output, wire') | (Right output, wire') <- stepped_wires]
