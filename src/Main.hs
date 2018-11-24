module Main where

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
bankWidth,moveDiff :: Float
bankWidth = 250
moveDiff = 5

-- | Render the game
render :: GameState -> Picture
render game = pictures $ snd $ renderPlane $ renderGround (game, [])

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


-- | Handle keyboard input
eventHandle :: Event -> GameState -> GameState
eventHandle (EventKey (Char 'a') _ _ _) game = movePlane game $ -moveDiff
eventHandle (EventKey (Char 'd') _ _ _) game = movePlane game $ moveDiff
eventHandle _ game = game

movePlane:: GameState -> Float -> GameState
movePlane game diff = game {planeLoc = newLoc}
    where
        oldLoc = planeLoc game
        newLoc = oldLoc + diff

-- | Hold the state of the game
data GameState = Game {
        planeLoc :: Float
    }

initialState :: GameState
initialState = Game {
    planeLoc = 0
}

step :: Float -> GameState -> GameState
step sec game = game

main :: IO ()
main = play window winBackground fps initialState render eventHandle step
