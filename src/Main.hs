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
speedupIndex, bankWidth, moveDiff, riverWidth, riverWidthHalf, enemyRadius, planeRadius ::
     Float
bankWidth = 250

riverWidth = fromIntegral winWidth - 2 * bankWidth

riverWidthHalf = riverWidth / 2

speedupIndex = 1.01

moveDiff = 3

enemyRadius = 15

planeRadius = 25

-- | Render the game
render :: GameState -> Picture
render game =
  pictures $
  snd $ renderPlane $ renderScore $ renderEnemies $ renderGround (game, [])

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

-- | Render enemies
renderEnemies :: (GameState, [Picture]) -> (GameState, [Picture])
renderEnemies (game, pics) = (game, pics ++ enemiesPictures)
  where
    ens = enemies game
    enemiesPictures = map renderEnemy ens
    renderEnemy :: Enemy -> Picture
    renderEnemy (x, y) = translate x y $ color red $ circleSolid enemyRadius

-- | Render the score
renderScore :: (GameState, [Picture]) -> (GameState, [Picture])
renderScore (game, pics) =
  (game, pics ++ [translate yTranslate 0 $ Text $ show $ score game])
  where
    yTranslate = -fromIntegral winWidth / 2

-- | Handle keyboard input
eventHandle :: Event -> GameState -> GameState
eventHandle (EventKey (Char 'a') state _ _) game = game {aState = state}
eventHandle (EventKey (Char 'd') state _ _) game = game {dState = state}
eventHandle (EventKey (Char 's') _ _ _) game = resetGame game
eventHandle _ game = game

-- | Because there is a Left/Right definition already
data Direction
  = DLeft
  | DRight

-- | Move the plane horizontally
movePlane :: Direction -> GameState -> GameState
movePlane DLeft game =
  if oldLoc > planeRadius - riverWidthHalf && not (stopped game)
    then game {planeLoc = oldLoc - moveDiff}
    else game
  where
    oldLoc = planeLoc game
movePlane DRight game =
  if oldLoc < riverWidthHalf - planeRadius && not (stopped game)
    then game {planeLoc = oldLoc + moveDiff}
    else game
  where
    oldLoc = planeLoc game

-- | Combine the steps
step :: Float -> GameState -> GameState
step sec game =
  snd $
  addEnemy $
  filterEnemies $
  stepPlane $ stepSpeedup $ stepCollision $ curry stepEnemies sec game

-- | Speedup game
stepSpeedup :: (Float, GameState) -> (Float, GameState)
stepSpeedup (sec, game) =
  (sec, game {gameSpeed = gameSpeed game * speedupIndex})

-- | Move plane according to pressed keys
stepPlane :: (Float, GameState) -> (Float, GameState)
stepPlane (sec, game) = (sec, newGame)
  where
    newGame = mvPlane (aState game) (dState game) game

mvPlane :: KeyState -> KeyState -> GameState -> GameState
mvPlane Up Down game = movePlane DRight game
mvPlane Down Up game = movePlane DLeft game
mvPlane _ _ game = game

-- | Remove out of screen enemies and increase score
filterEnemies :: (Float, GameState) -> (Float, GameState)
filterEnemies (sec, game) =
  (sec, game {enemies = filteredEnemies, score = scoreUp + score game})
  where
    filteredEnemies =
      filter (\(_, y) -> y > -(fromIntegral winHeight / 2)) (enemies game)
    scoreUp = length (enemies game) - length filteredEnemies

-- | Add next enemy according to difficulty index
addEnemy :: (Float, GameState) -> (Float, GameState)
addEnemy (sec, game) =
  if add > (riverWidthHalf - enemyRadius - difficulty game) &&
     not (stopped game)
    then (sec, newGame)
    else (sec, updatedGame)
  where
    [x, add] = take 2 $ randsrc game
    updatedGame = game {randsrc = drop 2 (randsrc game)}
    newGame = updatedGame {enemies = newEnemies}
    oldEnemies = enemies game
    newEnemies = (x, fromIntegral winHeight / 2) : oldEnemies

-- | Move enemies in the direction of y axis
stepEnemies :: (Float, GameState) -> (Float, GameState)
stepEnemies (sec, game) =
  if not $ stopped game
    then (sec, game {enemies = map moveEnemy (enemies game)})
    else (sec, game)
  where
    moveEnemy (x, y) = (x, y - gameSpeed game)

-- | determine collisions between plane and enemies
stepCollision :: (Float, GameState) -> (Float, GameState)
stepCollision (sec, game) =
  (sec, game {stopped = any isCollision $ enemies game})
  where
    px = planeLoc game
    isCollision pt =
      distance pt (px, -(fromIntegral winHeight / 2)) <
      enemyRadius + planeRadius

-- | Calculate the distance between two points
distance :: Enemy -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

-- | Hold the state of the game
data GameState = Game
  { planeLoc :: Float
  , gameSpeed :: Float
  , enemies :: [Enemy]
  , randsrc :: [Float]
  , aState :: KeyState
  , dState :: KeyState
  , stopped :: Bool
  , score :: Int
  , difficulty :: Float
  }

type Enemy = (Float, Float)

-- | The initial state of game
initialState :: GameState
initialState =
  Game
    { planeLoc = 0
    , gameSpeed = 1.5
    , enemies = []
    , randsrc = [] -- ^ Source of random numbers
    , aState = Up
    , dState = Up
    , stopped = False
    , score = 0
    , difficulty = 10 -- ^ difficulty ratio: the higher the harder
    }

-- | Reset game state to initial values, except the random source
resetGame :: GameState -> GameState
resetGame game = initialState {randsrc = randsrc game}

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
