module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random

type Point = (Float, Float)

-- | Window configuration
winHeight, winWidth, winXOffset, winYOffset, fps :: Int
winHeight = 500

winWidth = 800

winXOffset = 10

winYOffset = winXOffset

fps = 60

winBackground :: Color
winBackground = light blue

window :: Display
window = InWindow "River Raid" (winWidth, winHeight) (winXOffset, winYOffset)

-- | Game configuration
bankWidth, moveDiff, riverWidth, riverWidthHalf, enemyRadius, planeRadius ::
     Float
bankWidth = 250

riverWidth = fromIntegral winWidth - 2 * bankWidth

riverWidthHalf = riverWidth / 2

moveDiff = 3

enemyRadius = 15

planeRadius = 25

-- | Render the game
render :: GameState -> Picture
render game =
  pictures $ snd $ renderPlane $ renderEnemies $ renderGround (game, [])

-- | Render the ground around the river
renderGround :: (GameState, [Picture]) -> (GameState, [Picture])
renderGround (game, pics) =
  (game, pics ++ [makeBank xTranslate, makeBank $ -xTranslate])
  where
    xTranslate :: Float
    xTranslate = fromIntegral winWidth / 2 - (bankWidth / 2)
    makeBank x =
      translate x 0 $
      Color green $ rectangleSolid bankWidth $ fromIntegral winHeight

-- | Render the plane
renderPlane :: (GameState, [Picture]) -> (GameState, [Picture])
renderPlane (game, pics) = (game, pics ++ [plane])
  where
    yTranslate :: Float
    yTranslate = fromIntegral winHeight / 2
    plane =
      translate (planeLoc game) (-yTranslate) $
      Color black $ circleSolid planeRadius

renderEnemies :: (GameState, [Picture]) -> (GameState, [Picture])
renderEnemies (game, pics) = (game, pics ++ enemiesPictures)
  where
    ens = enemies game
    enemiesPictures = map renderEnemy ens
    renderEnemy :: Enemy -> Picture
    renderEnemy (x, y) =
      translate x y $ color red $ circleSolid enemyRadius

-- | Handle keyboard input
eventHandle :: Event -> GameState -> GameState
eventHandle (EventKey (Char 'a') state _ _) game = game {aState = state}
eventHandle (EventKey (Char 'd') state _ _) game = game {dState = state}
eventHandle (EventKey (Char 's') _ _ _) game = resetGame game
eventHandle _ game = game

-- | Move the plane horizontally if there is a space on the river
data Direction
  = DLeft
  | DRight

movePlane :: Direction -> GameState -> GameState
movePlane DLeft game =
  if oldLoc > planeRadius - riverWidthHalf
    then game {planeLoc = oldLoc - moveDiff}
    else game
  where
    oldLoc = planeLoc game
movePlane DRight game =
  if oldLoc < riverWidthHalf - planeRadius
    then game {planeLoc = oldLoc + moveDiff}
    else game
  where
    oldLoc = planeLoc game

step :: Float -> GameState -> GameState
step sec game =
  snd $ addEnemy $ filterEnemies $ stepPlane $ stepCollision  $ curry stepEnemies sec game

stepPlane :: (Float, GameState) -> (Float, GameState)
stepPlane (sec, game) = (sec, newGame)
  where
    newGame = mvPlane (aState game) (dState game) game

mvPlane :: KeyState -> KeyState -> GameState -> GameState
mvPlane Up Down game = movePlane DRight game
mvPlane Down Up game = movePlane DLeft game
mvPlane _ _ game = game

filterEnemies :: (Float, GameState) -> (Float, GameState)
filterEnemies (sec, game) =
  ( sec
  , game
      { enemies =
          filter (\(_, y) -> y > -(fromIntegral winHeight/2)) (enemies game)
      })

addEnemy :: (Float, GameState) -> (Float, GameState)
addEnemy (sec, game) =
  if add > (riverWidthHalf - enemyRadius - 3) && not (stopped game)
     then (sec, newGame)
     else (sec, updatedGame)
  where
    [x, add] = take 2 $ randsrc game
    updatedGame = game {randsrc = drop 2 (randsrc game)}
    newGame = updatedGame {enemies = newEnemies}
    oldEnemies = enemies game
    newEnemies = (x, fromIntegral winHeight / 2) : oldEnemies

stepEnemies :: (Float, GameState) -> (Float, GameState)
stepEnemies (sec, game) = if not $ stopped game 
                            then (sec, game {enemies = map moveEnemy (enemies game)})
                            else (sec, game)
  where
    moveEnemy (x, y) = (x, y - gameSpeed game)

stepCollision :: (Float, GameState) -> (Float,GameState)
stepCollision (sec, game) = (sec, game { stopped = any isCollision $ enemies game })
        where
        px = planeLoc game
        isCollision pt = distance pt (px, -(fromIntegral winHeight/2)) < enemyRadius + planeRadius

distance :: Enemy -> (Float,Float) -> Float
distance (x1,y1) (x2,y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

-- | Hold the state of the game
data GameState = Game
  { planeLoc :: Float
  , gameSpeed :: Float
  , enemies :: [Enemy]
  , randsrc :: [Float]
  , aState :: KeyState
  , dState :: KeyState
  , stopped :: Bool
  }

type Enemy = (Float, Float)

initialState :: GameState
initialState =
  Game
    { planeLoc = 0
    , gameSpeed = 1.1
    , enemies = []
    , randsrc = []
    , aState = Up
    , dState = Up
    , stopped = False
    }

resetGame :: GameState -> GameState
resetGame game = initialState { randsrc = randsrc game }

-- | Main function
main :: IO ()
main = do
  g <- newStdGen
  let is =
        initialState
          { randsrc =
              randomRs
                (enemyRadius - riverWidthHalf, riverWidthHalf - enemyRadius)
                g
          }
  play window winBackground fps is render eventHandle step
