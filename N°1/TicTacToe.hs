-- TicTacToe

module Main where

type Board = [Char]

showBoard :: Board -> String
showBoard str =
  "   |   |   \n" ++
  " " ++ str !! 0 : [] ++ " | " ++ str !! 1 : [] ++ " | " ++ str !! 2 : []  ++ " \n" ++
  "   |   |  \n" ++
  "---+---+---\n" ++
  "   |   |   \n" ++
  " " ++ str !! 3 : [] ++ " | " ++ str !! 4 : [] ++ " | " ++ str !! 5 : []  ++ " \n" ++
  "   |   |   \n" ++
  "---+---+---\n" ++
  "   |   |   \n" ++
  " " ++ str !! 6 : [] ++ " | " ++ str !! 7 : [] ++ " | " ++ str !! 8 : []  ++ " \n" ++
  "   |   |   \n"

-- isNumber
-- Returns true if the given char is a digit
isNumber :: Char -> Bool
isNumber c = filter (\x -> x == c) numbers /= []
  where
    numbers = [ '0' .. '9' ]

-- isValid
-- Returns true if the given move is valid for the given board
isValid :: Board -> Int -> Bool
isValid board p
  | p < 0 || p >= 9           = False   -- out of range
  | isNumber( board !! p )    = True    -- empty
  | otherwise                 = False   -- played

-- validModes
-- Returns the list of valid moves for a given board
validMoves :: [Char] -> [Int]
validMoves board
  | (winner board) /= ' ' = []
  | otherwise = [y | y <- [0..8], (isValid board y)]

-- move
-- Given a board, a player and a move position, returns a new board with the
-- new move applied
move :: Board -> Char -> Int -> Board
move (p:board) ch pos
  | pos > 0 = p:[] ++ (move board ch (pos - 1))
  | otherwise = ch:[] ++ board

-- scoreBoard
-- Score of a board (for our min/max tree)
-- Returns 1 if the player is a winner, -1 if not and 0 if a tie
scoreBoard :: Board -> Char -> Int
scoreBoard board player
  | (winner board) == ' '     = 0
  | (winner board) == player  = 1
  | otherwise                 = -1

-- scoreMove
-- Returns the score of a board if the given move is made
scoreMove :: Board -> Char -> Int -> Int
scoreMove board ch pos = scoreBoard ( move board ch pos ) ch

-- cmpMove
-- Compares to move tuples using the given function
cmpMove :: (Int -> Int -> Bool) -> (Int, Int) -> (Int, Int) -> (Int, Int)
cmpMove fn (m0, s0) (m1, s1)
  | fn s0 s1    = (m0, s0)
  | otherwise   = (m1, s1)

-- selectMove
selectMove :: (Int -> Int -> Bool) -> [(Int, Int)] -> (Int, Int)
selectMove fn (mv:moves) = foldr (cmpMove fn) mv moves

-- evaluateBoardMin
-- scores the board and returns minimum value move for the given board
evaluateBoardMin :: Board -> Int
evaluateBoardMin board
  | length (validMoves board) == 0    = scoreBoard board 'O'
  | otherwise = foldr max (head scores) (tail scores)
  where
  boards = map (move board 'O') (validMoves board)
  scores = map evaluateBoardMax boards

-- evaluateBoardMin
-- scores the board and returns maximum value move for the given board
evaluateBoardMax :: Board -> Int
evaluateBoardMax board
  | length (validMoves board) == 0    = scoreBoard board 'O'
  | otherwise = foldr min (head scores) (tail scores)
  where
  boards = map (move board 'X') (validMoves board)
  scores = map evaluateBoardMin boards

-- scoreMoves
-- Compute score for each possible move
-- Returns list of (Move, Score) tuples
scoreMoves :: Board -> [(Int, Int)]
scoreMoves board = zip (validMoves board) scores
  where
  boards = map (move board 'O') (validMoves board)
  scores = map evaluateBoardMax boards

-- maxScore
-- returns the move with the highest score
maxScore :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxScore (m0, s0) (m1, s1)
  | s0 > s1 = (m0, s0)
  | otherwise = (m1, s1)

-- bestMove
-- choose the best possible move
bestMove :: Board -> Int
bestMove board = move
  where
  scored = scoreMoves board
  (move, score) = foldr maxScore (head scored) (tail scored)

-- playerMove
-- Attempts to make a move on the board
-- Returns (True, newBoard) if a valid move, with newBoard being a new game board
-- Returns (False, board) if an invalid move, with the board being unchanged
playerMove :: Board -> Int -> (Bool, Board)
playerMove board pos
  | not (isValid board pos) = (False, board)
  | otherwise = (True, (move board 'X' pos))

-- winner
-- Checks if the board has a winning player
-- Returns '' if no winner, or the winner ('X' or 'O')
winner :: Board -> Char
winner b
  -- horizontal lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 1) && (b !! 0) == (b !! 2)) = b !! 0
  | (b !! 3) /= ' ' && ((b !! 3) == (b !! 4) && (b !! 3) == (b !! 5)) = b !! 3
  | (b !! 6) /= ' ' && ((b !! 6) == (b !! 7) && (b !! 6) == (b !! 8)) = b !! 6
  -- vertical lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 3) && (b !! 0) == (b !! 6)) = b !! 0
  | (b !! 1) /= ' ' && ((b !! 1) == (b !! 4) && (b !! 1) == (b !! 7)) = b !! 1
  | (b !! 2) /= ' ' && ((b !! 2) == (b !! 5) && (b !! 2) == (b !! 8)) = b !! 2
  -- diagonal lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 4) && (b !! 0) == (b !! 8)) = b !! 0
  | (b !! 2) /= ' ' && ((b !! 2) == (b !! 4) && (b !! 2) == (b !! 6)) = b !! 2
  -- no winner
  | otherwise = ' '

play :: Board -> IO ()
play board = do
  putStrLn ( showBoard board )
  if (length (validMoves board) == 0 || (winner board) /= ' ')
    then do
      putStrLn("Winner " ++ (show (winner board) ));
    else do
      putStrLn ( show (validMoves board) )
      putStrLn "Play? "
      pos <- getLine
      let (valid, b) = (playerMove board (read pos) )
      if (valid)
        then do putStrLn("\nOk\n");
                if (' ' /= (winner b))
                  then do
                    putStrLn("Winner " ++ (show (winner b) ));
                    putStrLn ( showBoard b )
                  else do
                    -- putStrLn( show (scoreMoves b) )
                    -- putStrLn( show (bestMove b) )
                    play (move b 'O' (bestMove b))
        else do putStrLn("\nInvalid Move!\n");
                play board  

main = do
  putStrLn "Tic Tac Toe!\n"
  play "012345678"