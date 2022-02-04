{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
main :: Program
main = walk3

type Program = IO ()

walk1 :: Program
walk1 = activityOf initialState handleEvent drawState1

walk2 :: Program
walk2 = activityOf initialState handleEvent drawState2

walk3 :: Program
walk3 = resettableActivityOf initialState handleEvent drawState2

resettableActivityOf ::
    world ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
resettableActivityOf initial change picture = activityOf initial (resettableChange initial change) picture

-- Zdeycydowałem, że puszczenie Esc należy zwyczajnie zignorować. 
-- Skoro naciśnięcie resetuje plansze, to puszczenie nie zrobi niczego sensownego.
resettableChange :: world -> (Event -> world -> world) -> Event -> world -> world
resettableChange initial change (KeyPress "Esc") world = initial
resettableChange initial change event world = change event world

data State = S {
  stPlayer :: Coord,
  stDir :: Direction
}

initialState :: State
initialState = S initialCoord initialDirection

handleEvent :: Event -> State -> State
handleEvent event s
  = let dir = getDirection event
  in case dir of 
    Just dir -> s { stPlayer = walkableCoord (stPlayer s) (adjacentCoord dir (stPlayer s)), stDir = dir }
    _        -> s
  where
    getDirection :: Event -> Maybe Direction
    getDirection (KeyPress "Right") = Just R
    getDirection (KeyPress "Up") = Just U
    getDirection (KeyPress "Left") = Just L
    getDirection (KeyPress "Down") = Just D
    getDirection _ = Nothing

drawState1 :: State -> Picture
drawState1 s = pictures [atCoord (stPlayer s) player1, pictureOfMaze]

drawState2 :: State -> Picture
drawState2 s = pictures [atCoord (stPlayer s) (player2 (stDir s)), pictureOfMaze]

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

pictureOfMaze :: Picture
pictureOfMaze = pictures [ let c = C x y in atCoord c (drawTile(maze c)) | x <- [-10..10], y <- [-10..10]]

data Direction = R | U | L | D
initialDirection :: Direction
initialDirection = D

data Coord = C { cX, cY :: Integer }
initialCoord :: Coord
initialCoord = C 1 (-1)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R c = c { cX = cX c + 1 }
adjacentCoord U c = c { cY = cY c + 1 }
adjacentCoord L c = c { cX = cX c - 1 }
adjacentCoord D c = c { cY = cY c - 1 }

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

data Tile = Wall | Ground | Storage | Box | Blank

walkableCoord :: Coord -> Coord -> Coord
walkableCoord curr new = if isWalkable new then new else curr
  where
    isWalkable :: Coord -> Bool
    isWalkable c = 
      case (maze c) of
        Storage -> True
        Ground  -> True
        _       -> False

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Storage = storage
drawTile Box = box
drawTile Ground = ground
drawTile Blank = blank

tile, wall, ground, storage, box :: Picture
tile = solidRectangle 1 1
spot = solidCircle 0.25
wall = colored (dark gray) tile
ground = colored (light (light gray)) tile
storage = colored white spot & ground
box = colored black tile

