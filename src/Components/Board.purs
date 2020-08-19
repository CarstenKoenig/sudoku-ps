module Components.Board where
  
import Prelude

import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import GameState (Field(..), GameState)
import GameState as GS
import Halogen (AttrName(..), ClassName(..), Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

type Query = Const Void
type Input = GameState Int
type Output = Void

-- the game component
board :: forall m. MonadAff m => MonadEffect m => Component HTML Query Input Output m
board = Hooks.component createComponent
  where
  createComponent { } currentState = Hooks.do
    -- render component
    Hooks.pure view
    where
    view =
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
                , HP.attr (AttrName "data-row") (show item.row)
                , HP.attr (AttrName "data-col") (show item.col)
                ]
                ( showValue item.value )
        showRow item =
            show item.row
        showColumn item =
            show item.col
        showValue Empty = []
        showValue (Fixed n) = [ HH.strong_ [ HH.text (show n)] ]
        showValue (Value n) = [ HH.text (show n) ]
        items = GS.fields currentState