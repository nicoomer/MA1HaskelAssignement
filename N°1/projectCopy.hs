
module Project 
( Board(initialize, click, flag, won, lost), 
  top) 
where

import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.Set as S
import System.Random

{- (x, y) coordinates -} 
type Position = (Int, Int) 

{- To identify a Click Move or a Flag Move -}
data MoveType = Click | Flag deriving (Eq, Show)
data Move = Move MoveType Position deriving (Eq, Show)

{- External information about cells, what the user sees -} 
data Cell
  = Masked       -- An unclicked Cell
  | Clicked      -- A clicked Cell
  | Flagged      -- A Cell with a Flag
  | Adjacent Int -- A discovered Cell, where the number represents
                 --   the number of adjacent mines
  deriving (Eq, Show)

{- Internal information about cells, hidden to the user -} 
data InternalCell
  = Mine          -- The Cell is a mine
  | IAdjacent Int -- The Cell is not a mine, but has Int adjacent mines
  deriving (Eq, Show)

{- Representaion of a Board -}
data MyBoard 
  = MyBoard 
    (Int, Int) -- Dimensions of the board (width, height)
    (Map Position Cell) -- The Map between the physical positions and the cells representation
    (Map Position InternalCell) -- The Map between the physical positions and the mined cells

instance Show MyBoard where
  show (MyBoard bounds board internal) = show board

-- The show instance must be highly customized to display a board in ASCII
class Show b => Board b where
  -- Create a board from seed, dimension and first click (avoid immediate loosing)
  initialize :: Int -> (Int, Int) -> (Int, Int) -> b
  -- Click a cell on the board (no effect if out-of-bounds)
  click :: (Int, Int) -> b -> b
  -- Flag a cell on the board (no effect if out-of-bounds)
  flag :: (Int, Int) -> b -> b
  -- Test if all the mines have been flagged and all the clean cells clicked 
  won :: b -> Bool
  -- Test if a mined cell has been clicked
  lost :: b -> Bool

{- Return the (width, height) bounds of the board -}
getBoardBounds :: MyBoard -> (Int, Int)
getBoardBounds (MyBoard bounds _ _) = bounds

{- Return the Map between the physical Positions and the Cells representation of the board -}
getBoardMap :: MyBoard -> Map Position Cell
getBoardMap (MyBoard _ board _) = board

{- Return the Map between the physical Positions and the mined Cells representation of the board -}
getInternalBoardMap :: MyBoard -> Map Position InternalCell
getInternalBoardMap (MyBoard _ _ internal) = internal

{- Return True if (x, y) coordinates are in range of (width, height), False otherwise -}
inBounds :: Position -> (Int, Int) -> Bool
inBounds (x, y) (width, height) = (x <= width && y <= height && x >= 1 && y >= 1)

{- Return the list of all adjacent cells of (x, y) -}
getAdjacentCells :: Position -> [Position]
getAdjacentCells (x, y) = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y+1), (x+1, y+1), (x+1, y), (x+1, y-1), (x, y-1)]

{- Return the list of all possible cells given the range (width, height) -}
getAllCells :: (Int, Int) -> [Position]
getAllCells (width, height) = getRow width height
  where
    getCellsOfRow 1 y = [(1, y)]
    getCellsOfRow x y = (x, y) : getCellsOfRow (x - 1) y
    getRow width 1 = getCellsOfRow width 1
    getRow width y = getCellsOfRow width y ++ getRow width (y - 1)

{- Given a Move (flagMove or clickMove + Position), 
Return True is the move is allowed, False otherwise -}
isValidMove :: Move -> MyBoard -> Bool
-- sentinel value
isValidMove (Move Flag (-1, -1)) (MyBoard (width, height) cells  _) = True
isValidMove (Move _ (x, y)) (MyBoard (width, height) cells  _) =
  if (not(inBounds (x, y) (width, height))) 
    then False
    else case cell of Just Masked -> True
                      Just _ -> False

  -- Retrive the value of the cell at the key : position (x, y) in the map. 
  where cell = M.lookup (x, y) cells

{- Return a random generator with the specified Int seed value -}
getRandomGen :: Int -> StdGen
getRandomGen seed = mkStdGen seed

{- Given a generator, return a tuple with the (rawTarget, nextGenerator) where
Int 'rawTarget' is not bounded & StdGen 'nextGenerator' is used for further random generations -}
getRandomInt :: StdGen -> (Int, StdGen)
getRandomInt stdGen = next stdGen

{- Return a random Int between (1, bound - 1) and another generator from a generator -}
randomIntWithinBound :: StdGen -> Int -> (Int, StdGen)
randomIntWithinBound stdGen bound = randomR (1, bound) stdGen

{- Try to place "numberOfMines" given the range (width, height) and 
the mines already placed in the list "listOfMines" + StdGen -}
setRandomMines :: Int -> (Int, Int) -> [Position] -> StdGen -> [Position]
setRandomMines 0 _ listOfMines _ = listOfMines
setRandomMines numberOfMines (width, height) listOfMines stdGen = do

      let (x, nextGen) = randomIntWithinBound stdGen width
      let (y, nextNextGen) = randomIntWithinBound nextGen height

      -- if we try to place the mine on another mine
      if elem (x, y) listOfMines 

        then setRandomMines numberOfMines (width, height) listOfMines nextNextGen
        -- Otherwise, you add the position of the mine to the listOfMines 
        -- + recursive call to place the (numberOfMines - 1) mines left

        else 
          setRandomMines (numberOfMines - 1) (width, height) ((x, y):listOfMines) nextNextGen

{- Given the listOfCells of the board, Return the list of tuple [(Position, Cell)] 
where the cell at the physical "Position" will have an "Cell" representation () -}
getBoardList :: [Position] -> [(Position, Cell)]
getBoardList listOfCells = foldr (\pos acc -> (pos, Masked):acc) [] listOfCells

{- Given the listOfCells of the board and the listOfMines placed on it,
Return the list of tuple [(Position, InternalCell)] where the cell 
at the physical Position will have an InternalCell representation -}
getInternalBoardList :: [Position] -> [Position] -> [(Position, InternalCell)]
getInternalBoardList listOfCells listOfMines = 
  foldr (\pos acc -> 

    -- If the current position is a mine, remember it in its internalCell
    if (elem pos listOfMines)  
      then (pos, Mine):acc 

    -- Otherwise, count the number of adjacent mines of the internal cell
    else 
      let adjacent = foldr (\adjPos acc -> if (elem adjPos listOfMines) then (1 + acc) else acc) 0 (getAdjacentCells pos) 

      -- at pos (int, int), InternalCell will have an "adjacent" number of mines, can be 0
      in (pos, IAdjacent adjacent):acc) [] listOfCells

{- Create a board given an Int seed and (width, height) dimensions -}
initializeBoard :: Int -> (Int, Int) ->(Int, Int) -> MyBoard
initializeBoard seed (width, height) (x, y) = MyBoard (width, height) (M.fromList boardList) (M.fromList internalBoardList) 
-- Transform the list "boardList" of type [(Position, Cell)] and the list
-- "internalBoardList" of type [(Position, InternalCell)] in maps with "fromList"

  where
   listOfCells = getAllCells (width, height)
   stdGen = getRandomGen seed

   (numberOfMines, nextGen) = randomIntWithinBound stdGen ((height * width) - 1)
   listOfMines = setRandomMines numberOfMines (width, height) [] nextGen

   boardList = getBoardList listOfCells
   internalBoardList = getInternalBoardList listOfCells listOfMines

propagate :: Position -> S.Set Position -> MyBoard -> (S.Set Position, MyBoard)
propagate pos visited board =
  if (S.member pos visited) 
    then (visited, board)
  else
    case cell of _ -> (visited, board)

  where 
    cell = M.lookup pos (getInternalBoardMap board)

{- Click a Position on the board -}
clickMove :: Position -> MyBoard -> MyBoard
clickMove pos board = 
  case cell of Just Mine -> (MyBoard (getBoardBounds board) (M.alter (\x -> Just Clicked) pos (getBoardMap board)) (getInternalBoardMap board))
               Just (IAdjacent an) -> newBoard

  where
    cell = M.lookup pos (getInternalBoardMap board)
    (_, newBoard) = propagate pos (S.empty) board

{- Flag a Position on the board. If the user flags a Flagged Cell, 
the Cell becomes Masked, if he flags a Masked Cell, it becomes Flagged -}
flagMove :: Position -> MyBoard -> MyBoard
flagMove (x, y) myBoard = 
  -- Build a new Board with the altered map
  case cell of Just Flagged -> (MyBoard (getBoardBounds myBoard) (M.alter (\c -> Just Masked) (x, y) (getBoardMap myBoard)) (getInternalBoardMap myBoard))
               Just Masked  -> (MyBoard (getBoardBounds myBoard) (M.alter (\c -> Just Flagged) (x, y) (getBoardMap myBoard)) (getInternalBoardMap myBoard))
  
  -- Retrive the value of the cell at the key : Position (x, y) in the map. 
  where cell = M.lookup (x, y) (getBoardMap myBoard)

{- Given a board, return True if the game is won -}
wonGame :: MyBoard -> Bool
wonGame board = 
  -- If all the Unclicked Cells are mines then 
  -- the difference between the 2 maps is null
  if (M.null (M.difference unclickedMap minesMap)) 
    then True

  else False

  where unclickedMap = M.filter (\cell -> cell == Masked || cell == Flagged) (getBoardMap board)
        minesMap = M.filter (\cell -> cell == Mine) (getInternalBoardMap board)

{- Given a board, return True if the game is lost -}
lostGame :: MyBoard -> Bool
lostGame board = 
  -- If among all the Clicked Cells, one of them is a mine  
  -- then the intersection between the 2 maps is not null
  if (not(M.null(M.intersection clickedMap minesMap))) 
    then True

  else False

  where clickedMap = M.filter (\cell -> cell == Clicked) (getBoardMap board)
        minesMap = M.filter (\cell -> cell == Mine) (getInternalBoardMap board)

instance Board MyBoard where
  initialize seed (width, height) (x, y) = MyBoard (width, height) (M.fromList boardList) (M.fromList internalBoardList) 
  -- Transform the list "boardList" of type [(Position, Cell)] and the list
  -- "internalBoardList" of type [(Position, InternalCell)] in maps with "fromList"
    where
      listOfCells = getAllCells (width, height)
      stdGen = getRandomGen seed

      (numberOfMines, nextGen) = randomIntWithinBound stdGen ((height * width) - 1)
      listOfMines = setRandomMines numberOfMines (width, height) [] nextGen

      boardList = getBoardList listOfCells
      internalBoardList = getInternalBoardList listOfCells listOfMines

  {- Flag a Position on the board. If the user flags a Flagged Cell, 
  the Cell becomes Masked, if he flags a Masked Cell, it becomes Flagged -}
  flag (x, y) myBoard = 
    -- Build a new Board with the altered map
    case cell of Just Flagged -> (MyBoard (getBoardBounds myBoard) (M.alter (\c -> Just Masked) (x, y) (getBoardMap myBoard)) (getInternalBoardMap myBoard))
                 Just Masked  -> (MyBoard (getBoardBounds myBoard) (M.alter (\c -> Just Flagged) (x, y) (getBoardMap myBoard)) (getInternalBoardMap myBoard))
    
    -- Retrive the value of the cell at the key : Position (x, y) in the map. 
    where cell = M.lookup (x, y) (getBoardMap myBoard)

  {- Given a board, return True if the game is won -}
  won board = 
    -- If all the Unclicked Cells are mines then 
    -- the difference between the 2 maps is null
    if (M.null (M.difference unclickedMap minesMap)) 
      then True

    else False

    where unclickedMap = M.filter (\cell -> cell == Masked || cell == Flagged) (getBoardMap board)
          minesMap = M.filter (\cell -> cell == Mine) (getInternalBoardMap board)

  {- Given a board, return True if the game is lost -}
  lost board = 
    -- If among all the Clicked Cells, one of them is a mine  
    -- then the intersection between the 2 maps is not null
    if (not(M.null(M.intersection clickedMap minesMap))) 
      then True

    else False

    where clickedMap = M.filter (\cell -> cell == Clicked) (getBoardMap board)
          minesMap = M.filter (\cell -> cell == Mine) (getInternalBoardMap board)

-- Create a main function by given a your initialize implementation
-- e.g.: main = top (initialize :: Int -> (Int, Int) -> (Int, Int) -> MyBoard)
top :: Board b => (Int -> (Int, Int) -> (Int, Int) -> b) -> IO ()
top cinit = do putStrLn "Enter a seed..."
               seed <- readLn
               putStrLn "Enter the width of the board"
               width <- readLn
               putStrLn "Enter the height of the board"
               height <- readLn
               putStrLn "First click..."
               loop $ cinit seed width height

-- A turn
loop :: Board b => b -> IO ()
loop board
  | won board  = putStrLn $ show board ++ "\n Gratz, you won!!!"
  | lost board = putStrLn $ show board ++ "\n Soz, you lost..."
  | otherwise  = do putStrLn $ show board
                    -- sentinel value to start the loop
                    newBoard <- flag_loop (Just (-1, -1)) board
                    putStrLn "Click ..."
                    coord <- readLn
                    loop $ click coord newBoard

-- Place flags
flag_loop :: Board b => Maybe (Int, Int) -> b -> IO b
flag_loop Nothing board = return board
flag_loop (Just coord) board = do putStrLn "Place a flag???"
                                  mcoord <- readLn
                                  flag_loop mcoord (flag coord board)
