-- | wire utilities that host state for current frame
-- generate new wires for the next frame based on input and
-- do not filter out inhibited wires
module NFInvaders.Util.Simulation.Collection.Statefull.Generated.Unfiltered where

import Control.Wire.Core       ( Wire
                               , mkGen
                               , stepWire)

import NFInvaders.Util.Netwire (stepWire')
import Control.Monad           (zipWithM)
import Data.Monoid             (Monoid)

-- | A wire to represent wire collections that holds internal state for the current frame
-- generates next frame from input
-- Does not out filter inhibited wires
wireCollection :: (Monad m, Monoid s)
               => (x -> Wire s e m a b)                       -- ^ generator for new arrows
               -> [Wire s e m a b]                            -- ^ existing arrows
               -> Wire s e m ([Either e a], [x]) [Either e b] -- ^ input for existing arrows and a list of new arrows, output for existing arrows
wireCollection arrow_generator arrows = mkGen $ \dt (input_list, new_arrow_bases) -> do
  let new_arrows = fmap arrow_generator new_arrow_bases
  stepped <- zipWithM (`stepWire` dt) arrows input_list
  return (Right (fmap fst stepped), wireCollection arrow_generator (fmap snd stepped ++ new_arrows))

-- | A wire to represent wire collections with a single input that holds internal state for the current frame
-- generates next frame from input
-- Does not out filter inhibited wires
multicastWireCollection :: (Monad m, Monoid s)
                        => (x -> Wire s e m a b)                     -- ^ generator for new arrows
                        -> [Wire s e m a b]                          -- ^ existing arrows
                        -> Wire s e m (Either e a, [x]) [Either e b] -- ^ input for existing arrows and a list of new arrows, output for existing arrows
multicastWireCollection arrow_generator arrows = mkGen $ \dt (input, new_arrow_bases) -> do
  let new_arrows = fmap arrow_generator new_arrow_bases
  stepped <- mapM (stepWire' dt input) arrows
  return (Right (fmap fst stepped), multicastWireCollection arrow_generator (fmap snd stepped ++ new_arrows))

-- | A wire to represent wire collections with no input that holds internal state for the current frame
-- generates next frame from input
-- Does not out filter inhibited wires
automatedWireCollection :: (Monad m, Monoid s)
                        => (x -> Wire s e m () b)      -- ^ generator for new arrows
                        -> [Wire s e m () b]           -- ^ existing arrows
                        -> Wire s e m [x] [Either e b] -- ^ list of new arrows, output for existing arrows
automatedWireCollection arrow_generator arrows = mkGen $ \dt new_arrow_bases -> do
  let new_arrows = fmap arrow_generator new_arrow_bases
  stepped <- mapM (stepWire' dt $ Right ()) arrows
  return (Right (fmap fst stepped), automatedWireCollection arrow_generator (fmap snd stepped ++ new_arrows))
