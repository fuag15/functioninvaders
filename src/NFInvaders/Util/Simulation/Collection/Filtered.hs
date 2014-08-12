-- | wire utilities that filter out inhibited wires
module NFInvaders.Util.Simulation.Collection.Filtered where

import Control.Wire.Core       ( Wire
                               , mkGen
                               , stepWire )

import NFInvaders.Util.Netwire ( stepWire'
                               , removeInhibited )

import Data.Monoid             (Monoid)

-- | A wire to represent wire collections, filteres out inhibited wires
wireCollection :: (Monad m, Monoid s)
               => Wire s e m [(Wire s e m a b, Either e a)] [(b, Wire s e m a b)] -- ^ a wire from a list of wires paired with an input to their output paired with the next wire
wireCollection = mkGen $ \dt arrow_list -> do
  stepped <- mapM (uncurry $ flip stepWire dt) arrow_list
  return (Right $ removeInhibited stepped, wireCollection)

-- | A wire to represent wire collections with a single input, filters out inhibited wires
multicastWireCollection :: (Monad m, Monoid s)
                        => Wire s e m ([Wire s e m a b], Either e a) [(b, Wire s e m a b)] -- ^ a wire from a list of wires and an input to thier output paired with the next wire
multicastWireCollection = mkGen $ \dt (arrow_list, input) -> do
  stepped <- mapM (stepWire' dt input) arrow_list
  return (Right $ removeInhibited stepped, multicastWireCollection)

-- | A wire to represent wire collections with no input, filters out inhibited wires
automatedWireCollection :: (Monad m, Monoid s)
                        => Wire s e m [Wire s e m () b] [(b, Wire s e m () b)] -- ^ a wire from a list of wires that take () as input to thier output paried with next wires
automatedWireCollection = mkGen $ \dt arrow_list -> do
  stepped <- mapM (stepWire' dt $ Right ()) arrow_list
  return (Right $ removeInhibited stepped, automatedWireCollection)
