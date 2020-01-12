module Components.Home (component) where

import Prelude

import Data.Foldable (null)
import Data.Maybe (Maybe(..))
import Data.Route (navigate)
import Data.Route as R
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { }

data Action
  = NoOp

type ChildSlots = ()

component :: forall q o m. MonadEffect m => H.Component HH.HTML q {} o m
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
  render state = showHome

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    NoOp ->
      pure unit

showHome :: forall w . HTML w Action
showHome =
  HH.div
    [ HP.class_ (ClassName "box") ] 
    [ HH.h1 [ HP.class_ (ClassName "subtitle") ] [ HH.text "Hello Halogen" ]
    ]