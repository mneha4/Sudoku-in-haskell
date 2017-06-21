--- Authors - Neha M (B14113), Siddhant Kumar (B14133)
--- Assignment 3 - CS302 

import Data.Maybe
import Data.List
import Data.Array
import Data.Char
import Debug.Trace
import System.IO
import System.Environment

-- types & constants
type SquareIdx = Int
type Val = Int
type Square = Maybe Int
type Unit = [Square]
type Move = (SquareIdx, Val)
type Board = Array Int Square


---- id's in a box of the sudoku puzzle ----
boxIndexes = [ [0, 1, 2, 9, 10, 11, 18, 19, 20]
          , [3, 4, 5, 12, 13, 14, 21, 22, 23]
          , [6, 7, 8, 15, 16, 17, 24, 25, 26] 
          , [27, 28, 29, 36, 37, 38, 45, 46, 47]
          , [30, 31, 32, 39, 40, 41, 48, 49, 50]
          , [33, 34, 35, 42, 43, 44, 51, 52, 53]
          , [54, 55, 56, 63, 64, 65, 72, 73, 74]
          , [57, 58, 59, 66, 67, 68, 75, 76, 77]
          , [60, 61, 62, 69, 70, 71, 78, 79, 80] ]

vals = [1..9]
unitSize = 9
boardSize = 80
squareIndexes = [0..boardSize]


-- Main -----
main = do
  -- get the command line arguments -- 
  args <- getArgs
  -- read the file and resolve the head of args first ($ - infix application)--
  contents <- readFile $ head args
  -- puzzle contains the input puzzle to be solved -- 
  let puzzle = create_board contents
  putStrLn "Input:"
  print_solution puzzle
  let solution = solve puzzle
  putStrLn "Solution:"
  print_solution solution


arrayFromList :: [a] -> Array Int a
--- (i, l !! i) means l[i] <-- i ----
arrayFromList l = array (0, len - 1) [(i, l !! i) | i <- [0..((length l) - 1)]]
  where len = length l

existent :: [Maybe a] -> [a]
existent = map fromJust . filter isJust

getSquare :: Board -> SquareIdx -> Square
getSquare = (!)

getSquares :: Board -> [SquareIdx] -> [Square]
getSquares board = map (getSquare board)


getRowOfIndex :: Board -> SquareIdx -> Unit
getRowOfIndex board i = getSquares board indexes
--- unitSize is 9 ---
  where row = div i unitSize
        --- take first unitsize elements of th list ---
        --- drop first unitsize*row elements of the list ---
        --- indexes will acontain all the indexes in the row ---
        indexes = take unitSize . drop (unitSize * row) $ squareIndexes

getColOfIndex :: Board -> SquareIdx -> Unit
getColOfIndex board i = getSquares board indexes
  where indexes = every unitSize $ drop (mod i unitSize) squareIndexes

getBoxOfIndex :: Board -> SquareIdx -> Unit
getBoxOfIndex board i = getSquares board indexes
  where indexes = head . filter (elem i) $ boxIndexes

getUnits :: Board -> SquareIdx -> [Unit]
getUnits board i = [getRowOfIndex board i, getColOfIndex board i, getBoxOfIndex board i]

--- solution finding ----

isSolved :: Board -> Bool
isSolved board = all isJust $ getSquares board squareIndexes

getEmptySquareIndexes :: Board -> [SquareIdx]
--- dot is used to make a new function out of any number of functions ----
--- returns those indexes which have 'Nothing' as a value ----
getEmptySquareIndexes board = filter (isNothing . getSquare board) squareIndexes

getLegalMovesForIdx :: Board -> SquareIdx -> [Move]
getLegalMovesForIdx board index = 
  let units = getUnits board index
      ---- nub removes duplicates from list ----    
      neighborVals = nub . existent . concat $ units
      valIsLegalMove = (`notElem` neighborVals)
  ---- here x will be the value----     
  in map (\x -> (index, x)) . filter valIsLegalMove $ vals

getSingleMoves :: Board -> [Move]
getSingleMoves board =
  let free = getEmptySquareIndexes board
      moves = map (getLegalMovesForIdx board) free
      ---- take only those moves which have only value for a index in th sudoku ----
      correctMoves = filter ((==1) . length) moves
  in concat correctMoves

makeMove :: Board -> (SquareIdx, Val) -> Board
---- make the changes in the board ---
makeMove board (index, val) =
	let board = board // [(index, Just val)]
		putStrLn board
	-- in board // [(index, Just val)]

solve :: Board -> Board
solve board
--- return board as solve has return type of board --- 
  | isSolved board = board
  | length correctMoves > 0 = solve $ makeMove board (head correctMoves)
  | otherwise = error "Could not solve board"
  where correctMoves = getSingleMoves board


-- Show the results ---

rowToLine :: [Square] -> String
rowToLine =
  let getChar Nothing = '.'
      getChar (Just x) = head $ show x
  in intersperseN 3 '|' . map getChar

print_solution :: Board -> IO ()
print_solution board = do 
  putStrLn ""
  let rows = chunk unitSize squareIndexes
      vals = map rowToLine . map (getSquares board) $ rows
      lines = intersperseN 3 "---+---+---" vals
  mapM_ putStrLn lines
  putStrLn ""

---- converts a string input into a sudoku board ----
create_board :: String -> Board
create_board input =
  let vals = concat $ lines input
      ---- definition of toSquare ----
      toSquare '.' = Nothing
      toSquare x = Just (digitToInt x)
      ---- map applies toSquare function to each element of vals ----
      list = map toSquare vals 
  in (arrayFromList list) :: Board

--- Printing utilities ---
every :: Int -> [a] -> [a]
every n = iter 0
  where iter _ [] = []
        iter 0 (x:xs) = x:(iter 1 xs)
        iter i (x:xs) = iter ((i+1) `mod` n) xs

intersperseN :: Int -> a -> [a] -> [a]
intersperseN n elem = iter (n-1)
  where iter _ [] = []
        iter _ (x:[]) = [x]
        iter 0 (x:y:xs) = x:elem:(iter (n-1) (y:xs))
        iter i (x:y:xs) = x:(iter ((i-1) `mod` n) (y:xs))

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l
  | n > 0 = (take n l) : (chunk n (drop n l))
  | otherwise = error "Negative n"
