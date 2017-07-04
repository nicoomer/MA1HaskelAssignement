
import Data.List
import Data.Map(Map)
import System.Random
import qualified Data.Map as M
import qualified Data.Set as S

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

{- Board Type Class -}
class Show b => Board b where
  -- Create a board from seed, dimension and first click
  initialize :: Int -> (Int, Int) -> Position -> b
  -- return a map of position where the mines are
  getMinesMap :: b -> Map Position InternalCell
  -- Click a cell on the board
  click :: Position -> b -> b
  -- Flag a cell on the board 
  flag :: Position -> b -> b
  -- Test if all the mines have been flagged and all the clean cells clicked 
  won :: b -> Bool
  -- Test if a mined cell has been clicked
  lost :: b -> Bool

{- Return the (width, height) bounds of the board -}
getBounds :: MyBoard -> (Int, Int)
getBounds (MyBoard bounds _ _) = bounds

{- Return the Map between the physical Position and the Cell representation of the board -}
getMap :: MyBoard -> Map Position Cell
getMap (MyBoard _ board _) = board

{- Return the Map between the physical Position and the InternalCell representation of the board -}
getInternalMap :: MyBoard -> Map Position InternalCell
getInternalMap (MyBoard _ _ internal) = internal

{- Return True if (x, y) coordinates are in range of (width, height), False otherwise -}
inBounds :: Position -> (Int, Int) -> Bool
inBounds (x, y) (width, height) = (x <= width && y <= height && x >= 1 && y >= 1)

{- Return the list of all adjacent cells of (x, y) -}
getAdjacentCells :: Position -> [Position]
getAdjacentCells (x, y) = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y+1), (x+1, y+1), (x+1, y), (x+1, y-1), (x, y-1)]

{- Return the list of all possible postions given the range (width, height) -}
getAllPositions :: (Int, Int) -> [Position]
getAllPositions (width, height) = getRow width height

  where
    getCellsOfRow 1 y = [(1, y)]
    getCellsOfRow x y = (x, y) : getCellsOfRow (x - 1) y

    getRow width 1 = getCellsOfRow width 1
    getRow width y = getCellsOfRow width y ++ getRow width (y - 1)

{- Given a Move (Flag || Click and a Position (x, y)), 
Return True is the move is allowed, False otherwise -}
isValidMove :: Move -> MyBoard -> Bool
isValidMove (Move move (x, y)) (MyBoard (width, height) cells  _) =

  if (not(inBounds (x, y) (width, height))) 
    then False

  else 
    if (move == Flag) -- To unFlagged Flagged Cells
      then case cell of Just Masked -> True
                        Just Flagged -> True
                        Just _ -> False

    else -- You cannot click on Flagged Cells
      case cell of Just Masked -> True
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
the mines already placed in the list "listOfMines" and the StdGen -}
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
getBoardList listOfCells = foldr (\position acc -> (position, Masked):acc) [] listOfCells

{- Given the listOfCells of the board and the listOfMines placed on it,
Return the list of tuple [(Position, InternalCell)] where the cell 
at the physical Position will have an InternalCell representation -}
getInternalBoardList :: [Position] -> [Position] -> [(Position, InternalCell)]
getInternalBoardList listOfCells listOfMines = 
  foldr (\position acc -> 

    -- If the current position is a mine, remember it in its internalCell
    if (elem position listOfMines)  
      then (position, Mine):acc 

    -- Otherwise, count the number of adjacent mines of the internal cell
    else 
      let adjacent = foldr (\adjPos acc -> if (elem adjPos listOfMines) then (1 + acc) else acc) 0 (getAdjacentCells position) 

      -- at position (int, int), InternalCell will have an "adjacent" number of mines, can be 0
      in (position, IAdjacent adjacent):acc) [] listOfCells

{- If there is a click at Position on MyBoard, remember the visited positions 
(in a Set : ordered and without duplicates) and propagate the click to other Cells -}
propagate :: Position -> S.Set Position -> MyBoard -> (S.Set Position, MyBoard)
propagate position visited board =

  -- If the position was already considered, return the set and the board
  if (S.member position visited) 
    then (visited, board) 

  else
    case cell of (Just (IAdjacent n)) -> if (n == 0) -- If there are no surrounding mines, add all the adjacent Cells to the set then recursively propagate the result thanks to propagateInList + update the cell showed to the user
                                            then propagateInList (getAdjacentCells position) (S.insert position visited) (MyBoard (getBounds board) (M.alter (\x -> Just (Adjacent n)) position (getMap board)) (getInternalMap board))

                                         else -- If there is adjacent mines don't consider surrounding cells as they will be considered later, just update the cell showed to the user
                                            propagateInList [] (S.insert position visited) (MyBoard (getBounds board) (M.alter (\x -> Just (Adjacent n)) position (getMap board)) (getInternalMap board))

                 _                    -> (visited, board)
                 -- If the Cell is a mine, don't propagate the result

  where 
    -- Retrive the value of the internal cell at the key : Position (x, y) in the map. 
    cell = M.lookup position (getInternalMap board)

{- Same as propagate but in a list, If the list of propagation is empty, return 
the current set. Otherwise, propagate the click to each element of the list -}
propagateInList :: [Position] -> S.Set Position -> MyBoard -> (S.Set Position, MyBoard)
propagateInList [] visited board = (visited, board)
propagateInList (position:acc) visited board =

  -- If the position (head of the list) is still in the bounds of the board
  if (inBounds position (getBounds board)) 
    -- propagate on the head of the List
    then let (visited', board') = propagate position visited board 
    in propagateInList acc visited' board'

  -- Recursive call on the tail of the List
  else
    propagateInList acc visited board

instance Board MyBoard where
  {- Create a board given an Int seed, (width, height) dimensions and a
  first click (x, y) that don't do anything if Position (x, y) is a Bomb -}
  initialize seed (width, height) (x, y)
    | not (elem (x, y) listOfMines) = click (x, y) board
    | otherwise = board

    where
      listOfCells = getAllPositions (width, height)
      stdGen = getRandomGen seed

      (numberOfMines, nextGen) = randomIntWithinBound stdGen ((height * width) - 1)
      listOfMines = setRandomMines numberOfMines (width, height) [] nextGen

      boardList = getBoardList listOfCells
      internalBoardList = getInternalBoardList listOfCells listOfMines

      -- Transform the list "boardList" of type [(Position, Cell)] and the list
      -- "internalBoardList" of type [(Position, InternalCell)] in maps with "fromList"

      board = MyBoard (width, height) (M.fromList boardList) (M.fromList internalBoardList)

  {- return a map of position where the mines are -}
  getMinesMap board = M.filter (\cell -> cell == Mine) (getInternalMap board)

  {- Click a Position on the board, propagate the click to adjacent cells -}
  click position myBoard = 

    -- If it is not a valid move, return the current board
    if (not(isValidMove (Move Click position) myBoard))
      then myBoard

    else 
      case cell of Just Mine -> (MyBoard (getBounds myBoard) (M.alter (\x -> Just Clicked) position (getMap myBoard)) (getInternalMap myBoard))
                   Just (IAdjacent an) -> newBoard

    -- Retrive the value of the Internal Cell at the key : Position (x, y) in the map.
    -- the updated board (if you click on a Cell with IAdjacent mines) is given thanks to the propagate function
    where
      cell = M.lookup position (getInternalMap myBoard)
      (_, newBoard) = propagate position (S.empty) myBoard

  {- Flag a Position on the board. If the user flags a Flagged Cell, 
  the Cell becomes Masked, if he flags a Masked Cell, it becomes Flagged -}
  flag position myBoard = 

    -- If it is not a valid move, return the current board
    if (not(isValidMove (Move Flag position) myBoard))
      then myBoard

    -- Build a new Board with the altered map
    else 
      case cell of Just Masked -> (MyBoard (getBounds myBoard) (M.alter (\c -> Just Flagged) position (getMap myBoard)) (getInternalMap myBoard))
                   Just Flagged -> (MyBoard (getBounds myBoard) (M.alter (\c -> Just Masked) position (getMap myBoard)) (getInternalMap myBoard))
      
    -- Retrive the value of the Cell at the key : Position (x, y) in the map. 
    where 
      cell = M.lookup position (getMap myBoard)

  {- Given a board, return True if the game is won -}
  won board = 

    -- If all the Unclicked Cells are mines then 
    -- the difference between the 2 maps is null
    if (M.null (M.difference minesMap flaggedMap)) 
      && (M.null (M.difference unclickedMap minesMap)) 
      then True

    else 
      False

    where 
      unclickedMap = M.filter (\cell -> cell == Masked) (getMap board)
      flaggedMap = M.filter (\cell -> cell == Flagged) (getMap board)
      minesMap = M.filter (\cell -> cell == Mine) (getInternalMap board)

  {- Given a board, return True if the game is lost -}
  lost board = 

    -- If among all the Clicked Cells, one of them is a mine  
    -- then the intersection between the 2 maps is not null
    if (not(M.null(M.intersection clickedMap minesMap))) 
      then True

    else 
      False

    where 
      clickedMap = M.filter (\cell -> cell == Clicked) (getMap board)
      minesMap = M.filter (\cell -> cell == Mine) (getInternalMap board)

{- Customisation of the show 
Instance for the Type board -}
instance Show MyBoard where
  show board = boardToString board

{- Highly custom function that turns a whole board into a String -}
boardToString :: MyBoard -> String
boardToString board = "    " ++ (printWithSeparator " " (map show [1..width]))   ++ "\n   " ++ -- Display the number of the column
                                (printWithSeparator "" (take width (repeat "+-"))) ++ "+\n" ++ -- Display the first +-+ separator
                                printRow 1 height board                                        -- Start to print Row 1
  where
    (width, height) = getBounds board

{- Given the number of the current row, the height of the board and the
board. Returns the rest of the String representation of the board  -}
printRow :: Int -> Int -> MyBoard -> String
printRow n height board = 

      -- Outside of the board
      if (n > height) 
        then ""

      else 
        -- Display the number of the row (one space less if 2 digits)
        (if (n >= 10)
          then show n ++ " |"

        else
          (show n ++ "  |"))

        ++ -- Still a String

        (if (lost board) -- Display the internal Cells if the game is lost, the external Cells otherwise
          then (printWithSeparator "" (internalCellRepresentationList (internalCellRow n board))) ++ "\n   " 

        else
          (printWithSeparator "" (cellRepresentationList (cellRow n board))) ++ "\n   ")

        ++ (printWithSeparator "" (take width (repeat "+-")))                    ++ "+\n"   ++ -- Display the +-+ separator
        printRow (n + 1) height board                                                          -- Recursive call to the next row

      where
        (width, height) = getBounds board 

{- Retrieve the map of Cells of the board and only consider those in the current Row -}
cellRow :: Int -> MyBoard -> Map Position Cell
cellRow n board = M.filterWithKey (\(x, y) s -> y == n) (getMap board)

{- Given a map of cells, returns a list with the ASCII representation of the cell -}
cellRepresentationList :: Map Position Cell -> [String]
cellRepresentationList mapOfCells = [cellToString s | (_, s) <- (M.toList mapOfCells)]

{- Retrieve the map of Cells of the board and only consider those in the current Row -}
internalCellRow :: Int -> MyBoard -> Map Position InternalCell
internalCellRow n board = M.filterWithKey (\(x, y) s -> y == n) (getInternalMap board)

{- Given a map of cells, returns a list with the ASCII representation of the cell -}
internalCellRepresentationList :: Map Position InternalCell -> [String]
internalCellRepresentationList mapOfCell = [internalCellToString s | (_, s) <- (M.toList mapOfCell)]

{- Given a String separator and List of String, it displays 
the element of the list separated by the separator. 
Example : printWithSeparator "-" ["Am","I","doing","this","right","?"]
          returns : "Am-I-doing-this-right-?" -}
printWithSeparator :: String -> [String] -> String
printWithSeparator separator list = concat (intersperse separator list)

{- The representation of Cells to String -}
cellToString :: Cell -> String
cellToString Flagged      = "F|"
cellToString (Adjacent n) = show n ++ "|"
cellToString _            = " |"

{- The representation of InternalCells to String -}
internalCellToString :: InternalCell -> String
internalCellToString Mine          = "M|"
internalCellToString (IAdjacent n) = show n ++ "|"

{- High-Order Function to collect the user's data and 
execute the initialize function with the given parameters -}
top :: Board b => (Int -> (Int, Int) -> Position -> b) -> IO ()
top cinit = do putStrLn "Enter a seed - Example : 42"
               seed <- readLn 
               putStrLn "Enter the width of the board"
               width <- readLn
               putStrLn "Enter the height of the board"
               height <- readLn
               putStrLn "First click - Example : (column N°, row N°)"
               firstClick <- readLn

               if (width <= 0 || height <= 0) 
                   then do putStrLn "The data you entered was invalid, please try again \n" 
                   >> top (initialize :: Int -> (Int, Int) -> Position -> MyBoard)

               else do
                   let newBoard = cinit seed (width, height) firstClick
                   info newBoard firstClick
                   loop $ newBoard

{- Display the number of mines and if the first click was a bomb -}
info :: Board b => b -> Position -> IO ()
info board firstClick = do putStr "\nNumber of Mines : "
                           putStrLn $ show (M.size (getMinesMap board))

                           if (M.member firstClick (getMinesMap board))
                             then putStrLn "The first click was a bomb, it didn't count :p \n"

                           else
                             putStrLn "Good Luck ! \n"

{- A turn, check if the game is won or lost
then ask the user to place a flag or to click -}
loop :: Board b => b -> IO ()
loop board
  | won board  = putStrLn $ show board ++ "\n Congratulations, you won =D"
  | lost board = putStrLn $ show board ++ "\n Sorry, you lost :("
  | otherwise  = do putStrLn $ show board
                    -- Sentinel value to start the loop
                    newBoard <- flag_loop (Just (-1, -1)) board
                    putStrLn "Click - Example : (column N°, row N°)"
                    coord <- readLn
                    loop $ click coord newBoard

{- Loop to place flags on the board -}
flag_loop :: Board b => Maybe Position -> b -> IO b
flag_loop Nothing board = do putStrLn $ show board 
                             return board
flag_loop (Just coord) board = do putStrLn "Place a flag ? - Example : Just(column N°, row N°) or Nothing"
                                  mcoord <- readLn
                                  flag_loop mcoord (flag coord board)

{- Main -}
main :: IO ()
main = top (initialize :: Int -> (Int, Int) -> Position -> MyBoard)