module Components.Home (component) where

import Prelude

import Components.GameBoard as Board
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State =
  { }

data Action
  = NoOp

type ChildSlots = 
  ( boardSlot :: Board.Slot Unit )

_boardSlot :: SProxy "boardSlot"
_boardSlot = SProxy

component :: forall q o m. MonadEffect m => H.Component HH.HTML q { } o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  initialState :: State
  initialState = 
    { }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = renderLayout

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    NoOp ->
      pure unit

renderLayout :: forall m.  MonadEffect m => H.ComponentHTML Action ChildSlots m
renderLayout =
  HH.section
    [ HP.class_ (ClassName "hero is-fullheight") ]
    [ HH.div
      [ HP.class_ (ClassName "hero-head") ]
      []
    , HH.div
      [ HP.class_ (ClassName "hero-body") ]
      [ HH.div
        [ HP.class_ (ClassName "container has-text-centered") ]
        [ HH.slot _boardSlot unit Board.component unit (\_ -> Nothing)
        ]
      ]
    , HH.div
      [ HP.class_ (ClassName "hero-foot") ]
      []
    ]