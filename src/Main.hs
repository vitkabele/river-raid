module Main where

import System.Random;
import Graphics.Gloss;
import Graphics.Gloss.Data.ViewPort;
import Graphics.Gloss.Interface.Pure.Game

type Point = (Float,Float)

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
bankWidth,moveDiff,riverWidth,riverWidthHalf,enemyRadius :: Float
bankWidth = 250
riverWidth = (fromIntegral winWidth) - 2 * bankWidth
riverWidthHalf = riverWidth/2
moveDiff = 5
enemyRadius = 15

-- | Render the game
render :: GameState -> Picture
render game = pictures $ snd $ renderPlane $ renderEnemies $ renderGround (game, [])

-- | Render the ground around the river
renderGround :: (GameState, [Picture]) -> (GameState, [Picture])
renderGround (game, pics) = (game, pics ++  [
        makeBank xTranslate, 
        makeBank $ -xTranslate
        ])
    where
        xTranslate :: Float
        xTranslate = ( (fromIntegral winWidth)/2 ) - (bankWidth/2)
        makeBank x = translate x 0 $ Color green $ rectangleSolid bankWidth $ fromIntegral winHeight

-- | Render the plane
renderPlane :: (GameState, [Picture]) -> (GameState, [Picture])
renderPlane (game, pics) = (game, pics ++ [
            plane 
        ])
    where
        yTranslate :: Float
        yTranslate = (fromIntegral winHeight)/2
        plane = translate (planeLoc game) (-yTranslate) $ Color black $ circleSolid 20

renderEnemies :: (GameState, [Picture]) -> (GameState,[Picture])
renderEnemies (game,pics) = (game, pics ++ enemiesPictures)
    where
        ens = enemies game
        enemiesPictures = map renderEnemy ens
        renderEnemy :: Enemy -> Picture
        renderEnemy (x,y) = translate x y $ color red $ circleSolid enemyRadius

-- | Handle keyboard input
eventHandle :: Event -> GameState -> GameState
eventHandle (EventKey (Char 'a') _ _ _) game = movePlane game $ -moveDiff
eventHandle (EventKey (Char 'd') _ _ _) game = movePlane game $ moveDiff
eventHandle _ game = game

-- | Move the plane horizontally
movePlane:: GameState -> Float -> GameState
movePlane game diff = game {planeLoc = newLoc}
    where
        oldLoc = planeLoc game
        newLoc = oldLoc + diff

step :: Float -> GameState -> GameState
step sec game = snd $ addEnemy $ curry stepEnemies sec game

addEnemy :: (Float, GameState) -> (Float, GameState)
addEnemy (sec, game) = if (round sec `mod` 4) == 0 
                        then (sec, newGame)
                        else (sec,game)
    where
        newGame = game { enemies = newEnemies, randsrc = drop 1 (randsrc game) }
        oldEnemies = enemies game
        newEnemies = ( head $ randsrc game, (fromIntegral winHeight)/2 ):oldEnemies

stepEnemies :: (Float,GameState) -> (Float,GameState)
stepEnemies (sec,game) = (sec, game { enemies = map moveEnemy (enemies game) })
    where
        moveEnemy (x,y) = (x, y - (gameSpeed game))

-- | Hold the state of the game
data GameState = Game {
        planeLoc :: Float,
        gameSpeed :: Float,
        enemies :: [Enemy],
        randsrc :: [Float]
    }

type Enemy = (Float, Float)

initialState :: GameState
initialState = Game {
    planeLoc = 0,
    gameSpeed = 1.1,
    enemies = [(-70,-50), (10,30)],
    randsrc = []
}


main :: IO ()
main = do
        g <- newStdGen
        let is = initialState { randsrc = randomRs (enemyRadius-riverWidthHalf,riverWidthHalf - enemyRadius) g }
        play window winBackground fps is render eventHandle step
