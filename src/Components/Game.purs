module Components.Game where
  
import Prelude

import Data.Const (Const)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

type Query = Const Void
type Input = Game
type Output = Void

type Game = Unit

-- the game component
game :: forall m. MonadAff m => MonadEffect m => Component HTML Query Input Output m
game = Hooks.component createComponent
  where
  createComponent { } initialGameState = Hooks.do
    -- setup hooks
    -- this component uses State of type Models.Game
    gameState /\ gameStateId <- Hooks.useState initialGameState

    -- render component
    Hooks.pure $ view gameState gameStateId
    where
    view gameState gameStateId =
        HH.div
            [ HP.class_ (ClassName "Game") ]
            [ HH.text "GAME"
            ]