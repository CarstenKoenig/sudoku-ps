module Components.GameBoard where

import Prelude

import Data.Array ((..))
import Data.Const (Const)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Slot = H.Slot (Const Unit) Message

data Message = None

data Action 
  = NoAction

type State =
  { }


component :: forall q i m. MonadEffect m => H.Component HH.HTML q i Message m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      }
    }
  where

  initialState :: State
  initialState = { }

  render :: State -> H.ComponentHTML Action () m
  render _ =
      HH.div
        [ HP.class_ $ ClassName "board" ] 
        [ HH.div
          -- for the padding-bottom trick see here: 
          -- https://stackoverflow.com/questions/19068070/how-to-style-a-div-to-be-a-responsive-square
          [ HP.class_ $ ClassName "board-content" ]
          (map renderRow (0..8))
        ]
    where
    renderRow rowNr =
      HH.div 
        [ HP.class_ $ ClassName "board-row" ]
        (map (renderCell rowNr) (0..8))
    renderCell rowNr colNr =
      HH.div
        [ HP.class_ $ ClassName "board-cell" ]
        []


  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    NoAction ->
      pure unit