import CodeWorld
main :: Program
main = program
type Program = IO ()

program :: Program
program = drawingOf pictureOfMaze

pictureOfMaze :: Picture
pictureOfMaze = pictures [ translated (fromIntegral x) (fromIntegral y) (drawTile(maze x y)) | x <- [-10..10], y <- [-10..10]]

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 3 = storage
drawTile 4 = box
drawTile n = ground

tile, wall, ground, storage, box :: Picture
tile = solidRectangle 1 1
spot = solidCircle 0.25
wall = colored (dark gray) tile
ground = colored (light (light gray)) tile
storage = colored white spot & ground
box = colored black tile
