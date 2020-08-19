module Components.Board where
  
import Prelude

import Data.Array ((..))
import Data.Const (Const)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (AttrName(..), ClassName(..), Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

type Query = Const Void
type Input = Board
type Output = Void

type Board = Unit

-- the game component
board :: forall m. MonadAff m => MonadEffect m => Component HTML Query Input Output m
board = Hooks.component createComponent
  where
  createComponent { } initialBoard = Hooks.do
    -- setup hooks
    -- this component uses State of type Models.Game
    boardState /\ boardStateId <- Hooks.useState initialBoard

    -- render component
    Hooks.pure $ view boardState boardStateId
    where
    view boardState boardStateId =
        HH.div
            [ HP.class_ (ClassName "Board") ]
            viewItems
        where
        viewItems =
            map viewItem items
        viewItem item =
            HH.div
                [ HP.class_ (ClassName "item") 
                , HP.attr 
                    (AttrName "style")
                    ("grid-row: " <> showRow item <> "; grid-column: " <> showColumn item <> ";")
                ]
                [ HH.text item.value ]
        showRow item =
            show item.row
        showColumn item =
            show item.column
        items =
            (\row col ->
                { row
                , column: col
                , value: show row <> "/" <> show col
                }
            )
            <$> (1..9) <*> (1..9)