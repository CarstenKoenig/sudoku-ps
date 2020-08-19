module GameState 
    ( GameState
    , Field (..)
    , Coord
    , Cell
    , initialize
    , fields
    , setValue
    , removeValue
    ) where

import Prelude

import Data.Array (zip, (..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))


newtype GameState a 
    = GameState (Map Coord (Field a))

derive instance functorGameState :: Functor GameState
derive instance newtypeGameState :: Newtype (GameState a) _


data Field a
    = Empty
    | Fixed a
    | Value a

derive instance functorField :: Functor Field


type Coord = 
    { row :: Int
    , col :: Int 
    }


type Cell a =
    { row :: Int
    , col :: Int
    , value :: a
    }


initialize :: Array (Maybe Int) -> GameState Int
initialize =
    GameState <<< Map.fromFoldable <<< zip coords <<< map toField
    where
    toField Nothing = Empty
    toField (Just n) = Fixed n
    coords = do
        row <- 1..9
        col <- 1..9
        pure { row, col }


fields :: forall a. GameState a -> Array (Cell (Field a))
fields (GameState m) =
    (\(Tuple coord field) -> 
        { row: coord.row
        , col: coord.col
        , value: field
        }) <$> Map.toUnfoldable m


setValue :: forall a. Coord -> a -> GameState a -> GameState a
setValue coord value (GameState m) = GameState $
    Map.insert coord (Value value) m


removeValue :: forall a. Coord -> GameState a -> GameState a
removeValue coord (GameState m) = GameState $
    Map.insert coord Empty m