module Update(
	runGame,
	moveBackwards,
	moveBox,
	movePlayer,
	updateBoard,
	eventHandler,
	stepLevel,
  stepGame
) where

import Graphics.Gloss(Point)
import Graphics.Gloss.Interface.Pure.Game
import World
import Types
import Square
import Data.Maybe (fromJust)
import Data.List(delete)
import LevelParser

runGame = do
  level <- readFile "levels/level001.lvl"
  play 
    (InWindow "Sokoban" (800, 600) (400, 400))
    white
    45
    (Game 1 (parseLevel level) False)
    gameToPicture
    gameEventHandler
    stepGame

moveBackwards :: Point -> Direction -> Point
moveBackwards p dir = movePoint p $ fromJust $ lookup dir opposites
  where opposites = zip [R,L,U,D] [L,R,D,U]

movePoint :: Point -> Direction -> Point
movePoint p@(x,y) dir =
    case dir of
      R -> (x+1, y)
      L -> (x-1, y)
      U -> (x, y+1)
      D -> (x, y-1)
      _ -> p

moveBox :: Direction -> Point -> Level Square -> Level Square
moveBox dir point world@(Level plr bxs wls sws) = world'
  where boxPoint   = movePoint point dir
        (Level plr' bxs' wls' sws') = fmap getPoint world
        world'
          | boxPoint `elem` bxs' = world
          | boxPoint `elem` wls' = world
          | boxPoint `elem` sws' = Level 
                                   (Player point)
                                   (map Box (boxPoint: delete point bxs'))
                                   wls
                                   sws
          | otherwise            = Level 
                                   (Player point)
                                   (map Box (boxPoint: delete point bxs'))
                                   wls
                                   sws

movePlayer :: Direction -> Point -> Level Square -> Level Square
movePlayer dir point w@(Level _ boxes walls switches)
  | point `elem` bxs  = moveBox dir point w
  | point `elem` wls  = w
  | point `elem` sws  = Level (Player point) boxes walls switches
  | otherwise         = Level (Player point) boxes walls switches
  where wPoints@(Level _ bxs wls sws) = fmap getPoint w

gameUpdateBoard = (flip updateBoard) . currentLevel

updateBoard :: Direction -> Level Square -> Level Square
updateBoard dir w = movePlayer dir p' w
  where p' = movePoint (getPoint $ player w) dir

gameEventHandler e (Game i lvl won) = Game i newLevel won  
  where newLevel = eventHandler e lvl

eventHandler :: Event -> Level Square -> Level Square
eventHandler e@(EventMotion coord) = id
eventHandler e@(EventKey key keyState mods coord) = 
  if keyState == Down then 
    case key of
                    (SpecialKey KeyUp)    -> updateBoard U
                    (SpecialKey KeyDown)  -> updateBoard D
                    (SpecialKey KeyLeft)  -> updateBoard L
                    (SpecialKey KeyRight) -> updateBoard R
                    _                     -> id
  else id

stepGame :: Float -> Game -> Game
stepGame _ = id

stepLevel :: Float -> Level Square -> Level Square
stepLevel _ = id