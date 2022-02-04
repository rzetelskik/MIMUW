{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Text as Text
main :: Program
main = walk5

type Program = IO ()

walk3 :: Program
walk3 = runActivity (resettable game)

walk4 :: Program
walk4 = runActivity (withStartScreen basicStartScreen (resettable game))

walk5 :: Program
walk5 = runActivity (withStartScreen etap4 (withLevels (withUndo (resettable game))))

game :: Activity State
game = Activity initialState handleEvent draw

data Activity world = Activity
  world
  (Event -> world -> world)
  (world -> Picture)
  
runActivity :: Activity world -> IO()
runActivity (Activity initial change picture) = activityOf initial change picture

data SSState world = StartScreen | Running world deriving Eq

withStartScreen :: Picture -> Activity world -> Activity (SSState world)
withStartScreen startScreen (Activity initial change picture)
  = Activity initial' change' picture'
  where
    initial' = StartScreen
    
    change' (KeyPress " ") StartScreen = Running initial
    change' _              StartScreen = StartScreen
    change' e              (Running s) = Running (change e s)
    
    picture' StartScreen = startScreen
    picture' (Running s) = picture s

etap4 :: Picture
etap4 = pictures [
  translated 0 3 (lettering "Correct mazes:"),
  correctMazes mazes,
  lettering "Bad mazes:",
  translated 0 (-3) (correctMazes badMazes),
  translated 0 (-3) (lettering "Press Space to start")]

basicStartScreen :: Picture
basicStartScreen = pictures [scaled 3 3 (lettering "Sokoban!"), translated 0 (-2) (lettering "Press Space to start")]

withLevels :: Advanceable world => Activity (WithUndo world) -> Activity (WithUndo world)
withLevels (Activity initial change picture) = Activity initial change' picture
  where
    change' (KeyPress "N") (WithUndo world stack) = WithUndo (advance world) []
    change' event          world = change event world
   
class Advanceable world where
  advance :: world -> world
    
data WithUndo a = WithUndo a [a]
withUndo :: Eq world => Activity world -> Activity (WithUndo world)
withUndo (Activity initial change picture) = Activity initial' change' picture'
  where
    initial' = WithUndo initial []
    
    change' (KeyPress "U") (WithUndo s stack)
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    change' e              (WithUndo s stack)
       | s' == s   = WithUndo s stack
       | otherwise = WithUndo s' (s:stack)
      where 
        s' = change e s
        
    picture' (WithUndo s _) = picture s

resettable :: Activity world -> Activity world
resettable (Activity initial change picture) 
  = Activity initial resettableChange picture
  where 
    resettableChange (KeyPress "Esc") _     = initial
    resettableChange event            world = change event world

data State = State {
  stPlayer  :: Coord,
  stDir     :: Direction,
  stBoxes   :: [Coord],
  stLvlNum  :: Integer,
  stMovNum  :: Integer
} deriving Eq

instance Advanceable State where
  advance = advanceLevel
  
advanceLevel :: State -> State
advanceLevel s | stLvlNum s < (listLength(mazes) - 1)
  = s { 
      stPlayer = mInit newMaze, 
      stDir = initialDirection,
      stBoxes = reachableTileCoords newMaze Box,
      stLvlNum = newLvlNum,
      stMovNum = 0
    }
    where
      newMaze = nth mazes newLvlNum
      newLvlNum = stLvlNum s + 1
advanceLevel s = s

currentMaze :: State -> Maze
currentMaze s = nth mazes (stLvlNum s) 

initialState :: State
initialState = let maze = nth mazes 0 in State (mInit maze) initialDirection (reachableTileCoords maze Box) 0 0

handleEvent :: Event -> State -> State
handleEvent _ s
  | isWinning s = s
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
isWinning s = allList (\c -> (mFunc (currentMaze s)) c == Storage) (stBoxes s)

draw :: State -> Picture
draw s = pictures [
  if isWinning s then win (stMovNum s) else blank,
  atCoord (stPlayer s) (player2 (stDir s)),
  pictureOfMaze (stBoxes s) (currentMaze s)]

win :: Integer -> Picture
win moves = pictures [
  translated 0 7 (scaled 2 2 (lettering "Level completed!")),
  translated 0 (-7) (lettering "Number of moves:"),
  translated 0 (-8) (lettering (Text.pack (show moves)))]

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
pictureOfMaze boxes (Maze _ mFunc mSize) 
  = pictures [
  let c = Coord x y in 
  atCoord c (drawTile (addBoxes boxes (removeBoxes mFunc) c))
  | x <- [-mSize..mSize], y <- [-mSize..mSize]]

correctMazes :: [Maze] -> Picture
correctMazes ms = pictureOfBools (mapList (\m -> isClosed m && isSane m) ms)

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k / 2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

isClosed :: Maze -> Bool
isClosed (Maze mInit mFunc _) 
  = (let st = mFunc mInit in st == Ground || st == Storage) && 
    isGraphClosed mInit (getNeighbours mFunc) (\c -> mFunc c /= Blank) 
 
isSane :: Maze -> Bool
isSane maze
  = listLength (reachableTileCoords maze Storage) >= listLength (reachableTileCoords maze Box)
    
reachableTileCoords :: Maze -> Tile -> [Coord]
reachableTileCoords (Maze mInit mFunc mSize) tile = filterList (\c -> reachable c mInit (getNeighbours mFunc)) allTileCoords
  where
    allTileCoords = [ c | x <- [-mSize..mSize], y <- [-mSize..mSize], let c = Coord x y, mFunc c == tile] 

getNeighbours :: MazeFunc -> Coord -> [Coord]
getNeighbours mFunc = filterList (\c -> mFunc c /= Wall) . getAdjacent

getAdjacent :: Coord -> [Coord]
getAdjacent c = [ adjacentCoord dir c | dir <- [R, U, L, D] ]

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = andList (mapList (\v -> reachable v initial neighbours) vs) 

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = bfs [initial] (||) neighbours (==v) []

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = bfs [initial] (&&) neighbours isOk []

bfs :: Eq a => [a] -> (Bool -> Bool -> Bool) -> (a -> [a]) -> (a -> Bool) -> [a] -> Bool
bfs [] _ _ _ _ = True
bfs (x:xs) op neighbours cond visited 
  = op (cond x) (bfs (appendList xs newNodes) op neighbours cond (x:visited))
  where
      newNodes = filterList (\a -> not (elemList a visited)) (neighbours x)

elemList :: Eq a => a -> [a] -> Bool
elemList el xs = foldList (\a b -> (a == el) || b) (False) xs

appendList :: [a] -> [a] -> [a]
appendList xs ys = foldList (:) ys xs

listLength :: [a] -> Integer
listLength xs = foldList (\a b -> b + 1) 0 xs

filterList :: (a -> Bool) -> [a] -> [a]
filterList f xs = foldList (\a b -> if f a then a:b else b) [] xs

nth :: [a] -> Integer -> a
nth xs n = head $ foldList ($) xs $ replicate (fromInteger n) tail

mapList :: (a -> b) -> [a] -> [b]
mapList f xs = foldList (\a b -> (f a):b) [] xs

andList :: [Bool] -> Bool
andList xs = foldList (&&) True xs

allList :: (a-> Bool) -> [a] -> Bool
allList f xs = foldList (\a b -> f a && b) True xs

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList f el xs = foldr f el xs

data Maze = Maze {
  mInit :: Coord,
  mFunc :: MazeFunc,
  mSize :: Integer -- Maze spans [-mSize..mSize] in both dimensions
}

type MazeFunc = Coord -> Tile
  
mazes :: [Maze]
mazes = [easyMaze, basicMaze, voidMaze]

badMazes :: [Maze]
badMazes = [noStoragesMaze, openMaze]

basicMaze :: Maze
basicMaze = Maze (Coord 1 (-1)) basicMazeFunc 5

basicMazeFunc :: MazeFunc
basicMazeFunc (Coord x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

voidMaze :: Maze
voidMaze = Maze (Coord 4 0) voidMazeFunc 6

voidMazeFunc :: MazeFunc
voidMazeFunc (Coord x y)
  | abs x > 5 || abs y > 5   = Blank
  | abs x < 2 && abs y < 2   = Blank
  | abs x == 5 || abs y == 5 = Wall
  | abs x == 2 && abs y < 2  = Wall
  | abs x < 2 && abs y == 2  = Wall
  | abs x == 2 && abs y == 2 = Storage
  | abs x == 3 && abs y == 3 = Box
  | otherwise                = Ground

easyMaze :: Maze
easyMaze = Maze (Coord 0 0) easyMazeFunc 4

easyMazeFunc :: MazeFunc
easyMazeFunc (Coord x y)
  | abs x > 3 || abs y > 3   = Blank
  | abs x == 3 || abs y == 3 = Wall
  | x == 0 && y == 0         = Storage
  | x == 1 && y == 1         = Box
  | otherwise                = Ground

noStoragesMaze :: Maze
noStoragesMaze = Maze (Coord 0 (-1)) noStoragesMazeFunc 5

noStoragesMazeFunc :: MazeFunc
noStoragesMazeFunc (Coord x y)
  | abs x > 4 || abs y > 4   = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Box
  | otherwise                = Ground
  
openMaze :: Maze
openMaze = Maze (Coord 0 0) openMazeFunc 5

openMazeFunc :: MazeFunc
openMazeFunc (Coord x y)
  | abs x > 4 || abs y > 4   = Blank
  | x == 0 && y == 0         = Storage
  | x == 1 && y == 1         = Box
  | otherwise                = Ground

removeBoxes :: MazeFunc -> MazeFunc
removeBoxes mFunc = mFunc'
  where
    mFunc' :: MazeFunc
    mFunc' = f . mFunc where f = \t -> if t == Box then Ground else t

addBoxes :: [Coord] -> MazeFunc -> MazeFunc
addBoxes boxes mFunc = mFunc'
  where
    mFunc' :: MazeFunc
    mFunc' c
      | any (c==) boxes = Box
      | otherwise       = mFunc c

data Direction = R | U | L | D deriving Eq
initialDirection :: Direction
initialDirection = D

data Coord = Coord { cX, cY :: Integer } deriving Eq
initialCoord :: Coord
initialCoord = Coord 4 0

atCoord :: Coord -> Picture -> Picture
atCoord (Coord x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R c = c { cX = cX c + 1 }
adjacentCoord U c = c { cY = cY c + 1 }
adjacentCoord L c = c { cX = cX c - 1 }
adjacentCoord D c = c { cY = cY c - 1 }

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

move :: Direction -> State -> State
move dir s
  | isBox new && isWalkable behindNew = s { stPlayer = new, stDir = dir, stBoxes = map (\c -> if c == new then behindNew else c)(stBoxes s), stMovNum = stMovNum s + 1 }
  | isWalkable new                    = s { stPlayer = new, stDir = dir, stMovNum = stMovNum s + 1 }
  | otherwise                         = s { stDir = dir }
  where
    new = adjacentCoord dir (stPlayer s)
    behindNew = adjacentCoord dir new
    
    isBox :: Coord -> Bool
    isBox c = elem c (stBoxes s) 
    
    isWalkable :: Coord -> Bool
    isWalkable c
      | isBox(c)                                                                     = False
      | let t = removeBoxes (mFunc (currentMaze s)) c in t == Storage || t == Ground = True
      | otherwise                                                                    = False
      
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
    


