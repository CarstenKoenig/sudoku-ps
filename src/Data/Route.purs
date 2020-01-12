module Data.Route
  ( Route(..)
  , codec
  , navigate
  , routeHref
 ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.HTML (IProp)
import Halogen.HTML.Properties as HP
import Routing.Duplex (RouteDuplex', print, root, end, default)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Hash (setHash)

data Route
  = Home

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

codec :: RouteDuplex' Route
codec = default Home $ root $ end $ sum
  { "Home": noArgs
  }

navigate :: forall m. MonadEffect m => Route -> m Unit
navigate = liftEffect <<< setHash <<< print codec

routeHref :: forall a props. Route -> IProp ( href :: String | props ) a
routeHref = print codec >>> HP.href