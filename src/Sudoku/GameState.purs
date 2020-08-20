module Sudoku.GameState 
    ( GameState
    , Field (..)
    , Coord
    , Cell
    , initialize
    , fields
    , setValue
    , getValue
    , removeValue
    , hasConflictAt
    ) where

import Prelude

import Data.Array (concat, elem, zip, (..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Sudoku.Coord as Coord


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


getValue :: forall a. Coord -> GameState a -> Maybe a
getValue coord (GameState m) = do
    field <- Map.lookup coord m
    case field of
        Fixed a -> pure a
        Value a -> pure a
        Empty   -> Nothing


removeValue :: forall a. Coord -> GameState a -> GameState a
removeValue coord (GameState m) = GameState $
    Map.insert coord Empty m


hasConflictAt :: forall a. Ord a => Coord -> GameState a -> Boolean
hasConflictAt coord gamestate = fromMaybe true $ do
    coordValue <- getValue coord gamestate
    pure $ coordValue `elem` values
    where
    values =
        getValues coords
    coords =
        Set.fromFoldable $
        concat 
            [ Coord.rowCoords coord
            , Coord.colCoords coord
            , Coord.blockCoords coord
            ]
    getValues = 
        Set.mapMaybe (flip getValue gamestate)
