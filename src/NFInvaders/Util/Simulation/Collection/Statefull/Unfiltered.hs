-- | wire utilities that hold internal state for the current frame do not filter out inhibited wires
module NFInvaders.Util.Simulation.Collection.Statefull.Unfiltered where

import Control.Wire.Core       ( Wire
                               , mkGen
                               , stepWire)

import NFInvaders.Util.Netwire (stepWire')
import Control.Monad           (zipWithM)
import Data.Monoid             (Monoid)

-- | A wire to represent wire collections that holds internal state for the current frame
-- Does not out filter inhibited wires
wireCollection :: (Monad m, Monoid s)
               => [Wire s e m a b]                                         -- ^ existing arrows
               -> Wire s e m ([Either e a], [Wire s e m a b]) [Either e b] -- ^ input for existing arrows and a list of new arrows, provides output for existing arrows
wireCollection arrows = mkGen $ \dt (input_list, new_arrows) -> do
  stepped <- zipWithM (`stepWire` dt) arrows input_list
  return (Right (fmap fst stepped), wireCollection (fmap snd stepped ++ new_arrows))

-- | A wire to represent wire collections with a single input that holds internal state for the current frame
-- Does not out filter inhibited wires
multicastWireCollection :: (Monad m, Monoid s)
                        => [Wire s e m a b]                                       -- ^ existing arrows
                        -> Wire s e m (Either e a, [Wire s e m a b]) [Either e b] -- ^ input for existing arrows and a list of new arrows, provides output for existing arrows
multicastWireCollection arrows = mkGen $ \dt (input, new_arrows) -> do
  stepped <- mapM (stepWire' dt input) arrows
  return (Right (fmap fst stepped), multicastWireCollection (fmap snd stepped ++ new_arrows))

-- | A wire to represent wire collections with no input that holds internal state for the current frame
-- Does not out filter inhibited wires
automatedWireCollection :: (Monad m, Monoid s)
                        => [Wire s e m () b]                         -- ^ existing arrows
                        -> Wire s e m [Wire s e m () b] [Either e b] -- ^ list of new arrows, provides output for existing arrows
automatedWireCollection arrows = mkGen $ \dt new_arrows -> do
  stepped <- mapM (stepWire' dt $ Right ()) arrows
  return (Right (fmap fst stepped), automatedWireCollection (fmap snd stepped ++ new_arrows))
