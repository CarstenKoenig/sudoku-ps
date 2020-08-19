module Components.Board where
  
import Prelude

import Data.Const (Const)
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import GameState (Field(..), GameState, Coord)
import GameState as GS
import Halogen (AttrName(..), ClassName(..), Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement as Inp

type Query = Const Void
type Input = GameState Int
data Output 
    = SetValue Coord (Maybe Int)

-- the game component
board :: forall m. MonadAff m => MonadEffect m => Component HTML Query Input Output m
board = Hooks.component createComponent
  where
  createComponent { outputToken } currentState = Hooks.do
    inputFocusCoord /\ inputFocusCoordId <- Hooks.useState Nothing
    -- render component
    Hooks.pure $ view inputFocusCoord { inputFocusCoordId }
    where
    view focusOn ids =
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
                ( showValue item )
        showRow item =
            show item.row
        showColumn item =
            show item.col
        showValue { value: Fixed n} = 
            [ HH.strong_ [ HH.text (show n)] ]
        showValue { row, col, value: Empty } = showInput { row, col } Nothing
        showValue { row, col, value: Value n } = showInput { row, col } (Just n)
        showInput coord value | Just coord == focusOn =
            [ HH.input
                [ HP.type_ InputNumber 
                , HP.class_ (ClassName "focused")
                , HP.value (maybe "" show value)
                , HE.onMouseOut (\_ -> Just (clearFocusOn coord))
                , HE.onInput (setValue coord)
                ]
            ]
        showInput coord value =
            [ HH.div 
                [ HP.class_ (ClassName "unfocused") 
                , HE.onMouseOver (\_ -> Just (setFocusOn coord)) 
                ]
                [ HH.text (maybe "" show value) ]
            ]
        items = GS.fields currentState
        clearFocusOn coord = 
            Hooks.modify_ ids.inputFocusCoordId (\curCoord -> if curCoord == Just coord then Nothing else curCoord)
        setFocusOn coord = 
            Hooks.put ids.inputFocusCoordId (Just coord)
        setValue coord event = Just do
            -- try to get a HTMLInput-Element from the events target (if given) and parse the value as an integer
            -- is 0 if something does not work out
            value <- liftEffect $ maybe (pure 0) (map round <<< Inp.valueAsNumber) (target event >>= Inp.fromEventTarget)
            if value >= 1 && value <= 9 then
                Hooks.raise outputToken (SetValue coord (Just value))
            else
                Hooks.raise outputToken (SetValue coord Nothing)