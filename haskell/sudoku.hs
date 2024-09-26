
-- define how to represent a board
-- main function to start the process (recursive)

type Row    = [Int]
type Column = [Int]
type Grid   = [Int]
type Board  = [Row]

easy =
 [ [5, 6, 3,  8, 7, 9,  2, 1, 4]
 , [7, 1, 9,  4, 2, 3,  6, 5, 8]
 , [2, 8, 4,  5, 6, 1,  3, 9, 7]

 , [4, 2, 6,  1, 0, 7,  9, 8, 3]
 , [0, 9, 5,  6, 3, 8,  4, 7, 2]
 , [8, 3, 7,  2, 9, 4,  1, 6, 5]

 , [9, 4, 0,  3, 0, 5,  7, 2, 6]
 , [6, 5, 0,  7, 4, 2,  8, 3, 9]
 , [3, 7, 2,  9, 8, 6,  5, 4, 1]
 ] :: [[Int]]


rows :: Board -> [Row]
rows board = board

cols :: Board -> [[Int]]
cols [] = repeat []
cols (row:rows) =
   zipWith (:) row $ cols rows


grids :: Board -> [Column]
grids board = 
   splitBy 9 $
   [ board !! (((x-1)*3+xi) -1) !! (((y-1)*3+yi) -1)  
   | x  <- [1..3] 
   , y  <- [1..3] 
   , xi <- [1..3] 
   , yi <- [1..3]
   ]

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy n xs = take n xs : splitBy n (drop n xs)


isSolved :: Board -> Bool
isSolved board =
 all comply (rows board) &&
 all comply (cols board) &&
 all comply (grids board) 

comply :: Row -> Bool
comply [] = True
comply (x:xs) = not (x `elem` xs)  && comply xs


insert :: Int -> (Int, Int) -> Board -> Board
insert num (row,col) board = let
 (rowsBefore, (targetRow: rowsAfter)) = splitAt row board 
 (colsBefore, (_:colsAfter))          = splitAt col targetRow 
 in
  rowsBefore ++ (colsBefore ++ num:colsAfter):rowsAfter


main :: IO ()
main = do
 let res = sudoku easy
 return () 


sudoku :: Board -> Maybe Board
sudoku board = go board (0,0)


go :: Board -> (Int, Int) -> Maybe Board
go board (9,_) = Nothing
go board pos@(i,j) =
  if isSolved board
    then Just board
  else 
   if alreadyFilled (board !! i !! j)
    then go board (next pos)
    else
     try 1 board pos
  
try :: Int -> Board -> (Int, Int) -> Maybe Board
try 10 _ _ = error "not possible to find solution" 
try num board pos =
    case go (insert num pos board) (next pos) of
     Nothing -> try (num + 1) board pos
     answer  -> answer

next :: (Int, Int) -> (Int, Int)
next (i,j) | j == 8 = (i+1,0)
           | otherwise = (i,j+1) 
