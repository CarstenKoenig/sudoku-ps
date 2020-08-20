module Components.Game where
  
import Prelude

import Components.Board as Board
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Sudoku.GameState (GameState)
import Sudoku.GameState as G
import Halogen (ClassName(..), Component, Slot)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

type Query = Const Void
type Input = GameState Int
type Output = Void

-- Slots for Child-Components
type Slots = 
    ( boardSlot :: Slot Board.Query Board.Input Unit
    )

-- Proxy to help bridge value/type levels and address for slots
_board = SProxy :: SProxy "boardSlot"

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
            [ HH.slot _board unit Board.board gameState handleBoardOutput
            ]
        where
        handleBoardOutput (Board.SetValue coord (Just value)) =
            Just $ Hooks.modify_ gameStateId (G.setValue coord value)
        handleBoardOutput (Board.SetValue coord Nothing) =
            Just $ Hooks.modify_ gameStateId (G.removeValue coord)