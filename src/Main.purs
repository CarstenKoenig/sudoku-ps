module Main where

import Prelude

import Algorithm.Generate as G
import Components.Game (game)
import Effect (Effect)
import GameState as GS
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = do
  -- initialize a random game
  sudoku <- GS.initialize <$> G.generateSudoku
  runHalogenAff do
    -- get a handle to the body after it's available
    body <- awaitBody
    -- run the game-component with the random game as input
    -- into the body of the browser-document
    runUI game sudoku body