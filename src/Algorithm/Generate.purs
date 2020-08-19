module Algorithm.Generate 
    ( generateSudoku
    ) where

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import generateSudokuImpl :: Fn2 (Maybe Int) (Int -> Maybe Int) (Effect (Array (Maybe Int)))

generateSudoku :: Effect (Array (Maybe Int))
generateSudoku = runFn2 generateSudokuImpl Nothing Just