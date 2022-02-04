{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
main :: Program
main = walk4

type Program = IO ()

walk3 :: Program
walk3 = runActivity (resettable game)

walk4 :: Program
walk4 = runActivity (resettable (withStartScreen game))

game :: Activity State
game = Activity initialState handleEvent draw

data Activity world = Activity
  world
  (Event -> world -> world)
  (world -> Picture)
  
runActivity :: Activity world -> IO()
runActivity (Activity initial change picture) = activityOf initial change picture

data SSState world = StartScreen | Running world

withStartScreen :: Activity world -> Activity (SSState world)
withStartScreen (Activity initial change picture)
  = Activity initial' change' picture'
  where
    initial' = StartScreen
    
    change' (KeyPress " ") StartScreen = Running initial
    change' _              StartScreen = StartScreen
    change' e              (Running s) = Running (change e s)
    
    picture' StartScreen = startScreen
    picture' (Running s) = picture s

startScreen :: Picture
startScreen = pictures [scaled 3 3 (lettering "Sokoban!"), translated 0 (-2) (lettering "Press Space to start")]

resettable :: Activity world -> Activity world
resettable (Activity initial change picture) 
  = Activity initial resettableChange picture
  where 
    resettableChange (KeyPress "Esc") _     = initial
    resettableChange event            world = change event world

data State = State {
  stPlayer :: Coord,
  stDir    :: Direction,
  stBoxes  :: [Coord],
  stMaze   :: Maze
}

initialState :: State
initialState = State initialCoord initialDirection initialBoxes pureInitialMaze

pureInitialMaze :: Maze
pureInitialMaze = removeBoxes initialMaze

handleEvent :: Event -> State -> State
handleEvent _ s
  | isWinning(s) = s
handleEvent event s
  = let dir = getDirection event
  in case dir of 
    Just dir -> move dir s
    _        -> s
  where
    getDirection (KeyPress "Right") = Just R
    getDirection (KeyPress "Up")    = Just U
    getDirection (KeyPress "Left")  = Just L
    getDirection (KeyPress "Down")  = Just D
    getDirection _                  = Nothing

isWinning :: State -> Bool
isWinning s = all(\c -> (stMaze s) c == Storage)(stBoxes s) 

draw :: State -> Picture
draw s = pictures [
  if isWinning s then win else blank,
  atCoord (stPlayer s) (player2 (stDir s)),
  pictureOfMaze (stBoxes s) (stMaze s)]

win :: Picture
win = pictures [
  translated 0 6 (scaled 2 2 (lettering "You won!")),
  translated 0 (-6) (lettering "Press Esc to restart")]

player1, smile, eye :: Picture
player1 = pictures [
  translated 0.2 0.2 eye,
  translated (-0.2) 0.2 eye, 
  translated 0 0.25 smile, 
  colored red (solidCircle 0.5)]
smile = arc (5*pi/4) (7*pi/4) 0.5
eye = solidCircle 0.075

player2 :: Direction -> Picture
player2 dir = rotated (rotation dir) player1
  where
    rotation :: Direction -> Double
    rotation R = pi/2
    rotation U = pi
    rotation L = 3*pi/2
    rotation _ = 0

pictureOfMaze :: [Coord] -> Maze -> Picture
pictureOfMaze boxes maze 
  = pictures [
  let c = Coord x y in 
  atCoord c (drawTile (addBoxes boxes maze c))
  | x <- [-10..10], y <- [-10..10]]

initialBoxes :: [Coord]
initialBoxes = [c | x <- [-10..10], y <- [-10..10], let c = Coord x y, initialMaze c == Box]

type Maze = Coord -> Tile

removeBoxes :: Maze -> Maze
removeBoxes maze = f . maze where f = \t -> if t == Box then Ground else t 

addBoxes :: [Coord] -> Maze -> Maze
addBoxes boxes maze = maze'
  where
    maze' c
      | any (c==) boxes = Box
      | otherwise       = maze c

data Direction = R | U | L | D
initialDirection :: Direction
initialDirection = D

data Coord = Coord { cX, cY :: Integer } deriving Eq
initialCoord :: Coord
initialCoord = Coord 1 (-1)

atCoord :: Coord -> Picture -> Picture
atCoord (Coord x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R c = c { cX = cX c + 1 }
adjacentCoord U c = c { cY = cY c + 1 }
adjacentCoord L c = c { cX = cX c - 1 }
adjacentCoord D c = c { cY = cY c - 1 }

initialMaze :: Maze
initialMaze (Coord x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

move :: Direction -> State -> State
move dir s
  | isBox new && isWalkable behindNew = s { stPlayer = new, stDir = dir, stBoxes = map (\c -> if c == new then behindNew else c)(stBoxes s)}
  | isWalkable new                    = s { stPlayer = new, stDir = dir }
  | otherwise                         = s { stDir = dir }
  where
    new = adjacentCoord dir (stPlayer s)
    behindNew = adjacentCoord dir new
    
    isBox :: Coord -> Bool
    isBox c = elem c (stBoxes s) 
    
    isWalkable :: Coord -> Bool
    isWalkable c
      | isBox(c)                                            = False
      | let t = (stMaze s) c in t == Storage || t == Ground = True
      | otherwise                                           = False
      
drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Storage = storage
drawTile Ground  = ground
drawTile Box     = box
drawTile Blank   = blank

tile, wall, ground, storage, box :: Picture
tile = solidRectangle 1 1
spot = solidCircle 0.25
wall = colored (dark gray) tile
ground = colored (light (light gray)) tile
storage = colored white spot & ground
box = colored black tile

