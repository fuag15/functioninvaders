{-# LANGUAGE TemplateHaskell #-}
-- | Basic game state data
module NFInvaders.Data.Game where

import Control.Lens                              ( makeLenses
                                                 , traverse
                                                 , mapMOf_
                                                 , (^.) )

import NFInvaders.Data.Actor.Bullet        as B  (Bullet(..))
import NFInvaders.Data.Actor.Invader       as I  (Invader(..))
import NFInvaders.Data.Actor.BraveDefender as BD (BraveDefender(..))
import NFInvaders.Data.Engine.Renderable   as R  (Renderable(render))

data Game = Game { _invaders             :: [Invader]
                 , _invaderBullets       :: [Bullet]
                 , _braveDefenderBullets :: [Bullet]
                 , _braveDefender        :: BraveDefender }

$(makeLenses ''Game)

instance Renderable Game where
  render = renderGame

renderGame :: Game -> IO ()
renderGame game = do
  mapMOf_ (invaders            .traverse) render game
  mapMOf_ (invaderBullets      .traverse) render game
  mapMOf_ (braveDefenderBullets.traverse) render game
  render $ game^.braveDefender
