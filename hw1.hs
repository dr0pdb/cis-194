{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise3

-- Exercise 1

botCircle, topCircle, midCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (translated 0 0 (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 8.5

trafficLight :: Int -> Picture
trafficLight 0 = botCircle green & midCircle black & topCircle black & frame
trafficLight 1 = botCircle black & midCircle yellow & topCircle black & frame
trafficLight 2 = botCircle black & midCircle black & topCircle red & frame
trafficLight 3 = botCircle black & midCircle yellow & topCircle red & frame


trafficController :: Integer -> Picture
trafficController t
  | t >=0 && t <=1 = trafficLight 0
  | t == 2 = trafficLight 1
  | t >=3 && t <=4 = trafficLight 2
  | otherwise = trafficLight 3

trafficLightAnimation :: Double -> Picture
trafficLightAnimation t = trafficController (round t `mod` 6)

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Picture -> Integer -> Picture
tree b 0 = b
tree b n = translated 0 1 (rotated (pi/10) (tree b (n-1)) & rotated (- pi/10) (tree b (n-1))) &
           path [(0,0),(0,1)]

blossom :: Double -> Picture
blossom t = colored yellow (solidCircle ((min t 10)/50))


animation :: Double -> Picture
animation t = tree (blossom t) 8
  
exercise2 :: IO ()
exercise2 = animationOf animation

-- Exercise 3

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box =     colored brown (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

         
pictureOfMaze :: Picture
pictureOfMaze = drawRows (-10)

drawRows :: Integer -> Picture
drawRows 11 = blank
drawRows r = drawColumn r (-10) & drawRows (r+1)

drawColumn :: Integer -> Integer -> Picture
drawColumn _ 11 = blank
drawColumn r c = drawTileAt r c & drawColumn r (c+1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze r c))


exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 
