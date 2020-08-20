module Sudoku.Coord 
    ( Coord
    , rowCoords
    , colCoords
    , blockCoords
    ) where

import Prelude

import Data.Array (filter)
import Data.Unfoldable1 (range)


type Coord = 
    { row :: Int
    , col :: Int 
    }


rowCoords :: Coord -> Array Coord
rowCoords coord =
    { row: coord.row, col: _ } <$> filter (_ /= coord.col) (range 1 9)


colCoords :: Coord -> Array Coord
colCoords coord =
    { row: _, col: coord.col } <$> filter (_ /= coord.row) (range 1 9)


blockCoords :: Coord -> Array Coord
blockCoords coord =
    filter (_ /= coord) $ toCoord <$> range 1 3 <*> range 1 3
    where
    toCoord rowDelta colDelta =
        { row: blockRow * 3 + rowDelta
        , col: blockCol * 3 + colDelta
        }
    blockRow = (coord.row - 1) `div` 3
    blockCol = (coord.col - 1) `div` 3