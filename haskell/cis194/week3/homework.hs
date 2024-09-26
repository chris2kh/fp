{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

wall, ground, storage, box :: Picture
wall =    colored grey (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = (colored blue (solidCircle 0.3)) & ground
box =     colored brown      (solidRectangle 1 1)

player :: Direction -> Picture
player dir =
    colored red (solidRectangle 0.5 0.5) & (translated dx dy (solidRectangle 0.4 0.4)) 
    where 
    (dx, dy)
      | dir == R = (0.4, 0) 
      | dir == U = (0, 0.4)
      | dir == L = ((-0.4), 0)
      | dir == D = (0, (-0.4))
    

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt r c))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)


drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (noBoxMaze (C r c)))
         
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

maze2 :: Coord -> Tile 
maze2 (C x y)
  | y > 7 || y < (-6) || 
    x <= (-8) || x >= 8 ||
    x >= (-7) && x <= (-5) && y == 7 ||
    x == 7 && y >= 0 = Blank
  | y == (-6) && x>= (-7) && x <= 7 ||
    x == (-7) && y <= 5 && y >= (-6) ||
    x == 7 && y <= (-1) && y >= (-6) ||
    x == 6 && y <= 7 && y >= (-1) ||
    x >= (-4) && x <= 6 && y == 7 ||
    x >= (-7) && x <= (-4) && y == 6 ||
    x >= (-6) && x <= (-4) && y == 3 || -- boy
    x == (-4) && y <= 2  && y >= (-2)|| 
    y == 2 && x >= (-4) && x <= 0 = Wall
  | x == (-2) && y == 4 || 
    x == (-5) && y == (-3) ||
    x >= (-2) && x <= 0 && y == -3 = Box
  | x == (-5) && y == 4 ||
    x == (-5) && y == 0 ||
    x == (-2) && y == 0 ||
    x == 4 && y == (-4) ||
    x == 1 && y == (-5) ||
    x == (-5) && y == 3  = Storage
  | otherwise                = Ground

mazeSelected :: Coord -> Tile
mazeSelected = maze2

data Direction = 
    R 
  | U 
  | L 
  | D 
  deriving Eq

data Coord = C Integer Integer deriving Eq


initialPlayerCoord :: Coord
initialPlayerCoord = C (-3) (3)

fixedMazeCoord :: Coord
fixedMazeCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord dir (C x y)
  | tile == Ground || tile == Storage = nextCoord
  | otherwise = (C x y) -- cannot move
   where 
     nextCoord
       | dir == R = (C (x + 1) y)
       | dir == U = (C x (y + 1))
       | dir == L = (C (x - 1) y)
       | dir == D = (C x (y - 1))
     tile = mazeSelected nextCoord

updateState :: Direction -> (Coord, Picture) -> (Coord, Picture)
updateState dir (currentCoord, currentPic) =
  if canMove then (newCoord, newPicture) else (currentCoord, currentPic)
  where 
    canMove = newCoord /= currentCoord
    newCoord = adjacentCoord dir currentCoord
    newPicture = player dir

handleEvent :: Event -> (Coord, Picture) -> (Coord, Picture)
handleEvent (KeyPress key) currentState =
    case key of 
       "Right" -> updateState R currentState
       "Up"    -> updateState U currentState
       "Left"  -> updateState L currentState
       "Down"  -> updateState D currentState
       --"Esc"   -> (initialPlayerCoord, player R)
       _       -> currentState
handleEvent _ currentState = currentState


drawState :: (Coord, Picture) -> Picture
drawState (updatedCoord, updatedPlayer) = atCoord updatedCoord updatedPlayer & atCoord fixedMazeCoord pictureOfMaze


resetableActivityOf :: 
   world -> 
  (Event -> world -> world) -> 
  (world -> Picture) -> 
  IO () 
resetableActivityOf initialState handle draw =
    activityOf initialState handle' draw
    where handle' (KeyPress key) s
            | key /= "Esc" = handle (KeyPress key) s
            | otherwise = initialState
          handle' _ s = s

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")


data State world = 
  StartScreen
  | Running world


data Activity world =
  Activity
    world
    (Event -> world -> world)
    (world -> Picture)
    
resetable (Activity state0 handle draw) =
  Activity state0 handle' draw
    where
      handle' (KeyPress "Esc") _ = Running (initialPlayerCoord, player R)
      handle' e s = handle e s
  
 
withStartScreen (Activity state0 handle draw) =
  Activity StartScreen handle' draw'
  where 
    handle' (KeyPress " ") StartScreen = Running state0
    handle' _ StartScreen = StartScreen
    handle'  e (Running s) = Running (handle e s)
    
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

runActivity :: Activity s -> IO ()
runActivity (Activity initialGame handle draw)
  = activityOf initialGame handle draw


--initialInteraction = (Activity ((initialPlayerCoord, player R) handleEvent drawState))




--main :: IO ()
--main = runActivity (resetable (withStartScreen initialInteraction))



-- recusive data types
data List a = Empty | Entry a (List a)

boxesCoords :: List Coord
boxesCoords = Entry (C 2 2) (Entry (C 3 3) (Entry (C 4 4) Empty))

firstBox :: List Coord -> Picture
firstBox Empty = blank
firstBox (Entry c _) = atCoord c (drawTile Box)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes Empty = blank
pictureOfBoxes l = reduce (mapList (\c -> atCoord c (drawTile Box)) l)

movingBoxes :: Activity (List Coord)
movingBoxes = Activity boxesCoords handle draw
  where
    draw = pictureOfBoxes
    handle (KeyPress key) s
        | key == "Right" = mapList (adjacentCoord R) s
        | key == "Up"    = mapList (adjacentCoord U) s
        | key == "Left"  = mapList (adjacentCoord L) s
        | key == "Down"  = mapList (adjacentCoord D) s
    handle _ s      = s

moveAllBoxes :: Direction -> List Coord -> List Coord
moveAllBoxes _ Empty = Empty
moveAllBoxes d (Entry c cs) = Entry (adjacentCoord d c) (moveAllBoxes d cs)


reduce :: List Picture -> Picture
reduce Empty = blank
reduce (Entry x xs) = x & (reduce xs)


mapList _ Empty = Empty
mapList f (Entry x xs) = Entry (f x) (mapList f xs)

initialBoxesCoord :: List Coord
initialBoxesCoord = Empty

append :: Coord -> List Coord -> List Coord
append c l = (Entry c l)

isBox :: Coord -> Bool
isBox coord =
  mazeSelected coord == Box 

initialBoxes :: List Coord
initialBoxes =
  helper (-10) (-10) Empty
  where
    helper _ 11 list = list -- end of grid
    helper 11 n list = helper (-10) (n+1) list -- end of row, move to next one
    helper x y list
      | isBox coord = helper (x+1) y (append coord list)
      | otherwise = helper (x+1) y list
      where coord = (C x y)
        

noBoxMaze:: Coord -> Tile
noBoxMaze coord =
  case mazeSelected coord of
    Box -> Ground
    x   -> x


atList :: List Coord -> Coord -> Bool
atList Empty c = False
atList (Entry x xs) c =  x == c || atList xs c

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes list coord =
  if atList list coord 
    then Box
    else noBoxMaze coord


data State1 = 
  InitialState
  | State1 Coord Direction (List Coord) 


wonMessage :: Picture
wonMessage = scaled 2 2 (lettering "You win!")

draw1 :: State1 -> Picture
draw1 s =
  if isWon s then wonMessage else draw2 s


draw2 :: State1 -> Picture
draw2 (State1 coord dir boxes) =
      atCoord coord (player dir)
    & pictureOfBoxes boxes
    & atCoord fixedMazeCoord pictureOfMaze


updateCoord :: Direction -> Coord -> Coord
updateCoord dir (C x y) =
  next
  where
    next
      | dir == R = (C (x + 1) y)
      | dir == U = (C x (y + 1))
      | dir == L = (C (x - 1) y)
      | dir == D = (C x (y - 1))

updateBoxPosition :: Direction -> Coord -> List Coord -> Coord
updateBoxPosition  dir coord boxes
  | tile == Ground || tile == Storage = nextCoord
  | otherwise = coord
  where
    nextCoord = updateCoord dir coord
    tile = mazeWithBoxes boxes nextCoord


updatePlayerPosition :: State1 -> Coord
updatePlayerPosition (State1 coord dir boxes)
  | tile == Ground || tile == Storage = nextCoord
  | tile == Box && ((updateBoxPosition dir nextCoord boxes) /= nextCoord) = nextCoord
  | otherwise = coord
  where
    nextCoord = updateCoord dir coord
    tile = mazeWithBoxes boxes nextCoord

updateBoxes :: Direction -> Coord -> List Coord -> List Coord
updateBoxes dir coord list =
  mapList (\c -> if c == coord then (updateBoxPosition dir coord list) else c) list
  
updateState1 :: State1 -> State1
updateState1 (State1 coord dir boxes) =
  if canMove then state' else (State1 coord dir boxes)
  where
    canMove  = coord' /= coord
    coord'   = updatePlayerPosition (State1 coord dir boxes)
    boxes'   = updateBoxes dir coord' boxes
    state'   = (State1 coord' dir boxes')
    

handleEvent1 :: Event -> State1 -> State1
handleEvent1 (KeyPress key) (State1 coord dir boxes)
  |  key == "Right" = updateState1 (State1 coord R boxes)
  | key == "Up" = updateState1 (State1 coord U boxes)
  | key == "Left" = updateState1 (State1 coord L boxes)
  | key == "Down"  = updateState1 (State1 coord D boxes)
  | otherwise = (State1 coord dir boxes)
  
handleEvent1 _ s = s


initialInteraction1 = (Activity (State1 initialPlayerCoord R initialBoxes) handleEvent1 draw1)

runActivity1 :: Activity s -> IO ()
runActivity1 (Activity state handle draw) = 
  activityOf state handle draw

isOnStorage :: Coord -> Bool 
isOnStorage coord =
  (mazeSelected coord) == Storage

allList :: List Bool -> Bool
allList Empty = True
allList (Entry x xs) = x && allList xs

isWon :: State1 -> Bool
isWon (State1 _ _ boxes) =
  allList $ mapList isOnStorage boxes




main :: IO ()
main = runActivity1 (withStartScreen initialInteraction1)
