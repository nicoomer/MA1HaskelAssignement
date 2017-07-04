import Control.Monad
import Control.Monad.State

import Data.Either
import Data.List

data Symbol = X | O deriving (Eq,Show)
type Square = Either Int Symbol
type Board = [[Square]]
data GameState = Game { board :: Board }

initialGameState :: GameState
initialGameState = Game (map (map Left) [[1,2,3],[4,5,6],[7,8,9]])

-- print the board
showSquare = either (\n -> " " ++ show n ++ " ") (concat . replicate 3 . show)

showBoard :: Board -> String
showBoard board =
      unlines . surround "+---+---+---+"
    . map (concat . surround "|". map showSquare)
    $ board
    where
    surround x xs = [x] ++ intersperse x xs ++ [x]

printBoard = putStr . showBoard