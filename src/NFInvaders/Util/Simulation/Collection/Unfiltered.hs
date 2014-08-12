-- | wire utilities that do not filter out inhibited wires
module NFInvaders.Util.Simulation.Collection.Unfiltered where

import Control.Wire.Core       ( Wire
                               , mkGen
                               , stepWire)

import NFInvaders.Util.Netwire (stepWire')
import Data.Monoid             (Monoid)

-- | A wire to represent wire collections
-- Does not filter out inhibited wires
wireCollection :: (Monad m, Monoid s)
               => Wire s e m [(Wire s e m a b, Either e a)] [(Either e b, Wire s e m a b)] -- ^ a wire from a list of wires paired with an input to their output paired with the next wire
wireCollection = mkGen $ \dt arrow_list -> do
  stepped <- mapM (uncurry $ flip stepWire dt) arrow_list
  return (Right stepped, wireCollection)

-- | A wire to represent wire collections with a single input
-- Does not filter out inhibited wires
multicastWireCollection :: (Monad m, Monoid s)
                        => Wire s e m ([Wire s e m a b], Either e a) [(Either e b, Wire s e m a b)] -- ^ a wire from a list of wires and an input to thier output paired with the next wire
multicastWireCollection = mkGen $ \dt (arrow_list, input) -> do
  stepped <- mapM (stepWire' dt input) arrow_list
  return (Right stepped, multicastWireCollection)

-- | A wire to represent wire collections with no input
-- Does not filter out inhibited wires
automatedWireCollection :: (Monad m, Monoid s)
                        => Wire s e m [Wire s e m () b] [(Either e b, Wire s e m () b)] -- ^ a wire from a list of wires that take () as input to thier output paried with next wires
automatedWireCollection = mkGen $ \dt arrow_list -> do
  stepped <- mapM (stepWire' dt $ Right ()) arrow_list
  return (Right stepped, automatedWireCollection)
