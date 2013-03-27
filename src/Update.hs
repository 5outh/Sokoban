module Update(
	runGame,
	moveBackwards,
	moveBox,
	movePlayer,
	updateBoard,
	gameEventHandler,
	stepLevel,
  stepGame
) where

import Graphics.Gloss(Point)
import Graphics.Gloss.Interface.Pure.Game
import World
import Types
import Square
import Save
import Data.Maybe (fromJust)
import Data.List(delete)
import LevelParser
import Control.Monad.Trans
import Control.Monad.Trans.State

runGame = loadGame >>= \game ->
  play 
    (InWindow "Sokoban" (800, 600) (400, 400))
    white
    45
    game
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

gameEventHandler :: Event -> Game -> Game
gameEventHandler e@(EventMotion _) g = id g
gameEventHandler e@(EventKey key _ _ _) g@(Game i lvl won) = case key of
  (SpecialKey _) -> Game i (movementHandler e lvl) won
  (Char       _) -> globalUpdateHandler e g 
  
movementHandler :: Event -> Level Square -> Level Square
movementHandler e@(EventKey key keyState _ _) = 
  if keyState == Down then 
    case key of
                    (SpecialKey KeyUp)    -> updateBoard U
                    (SpecialKey KeyDown)  -> updateBoard D
                    (SpecialKey KeyLeft)  -> updateBoard L
                    (SpecialKey KeyRight) -> updateBoard R
                    _                     -> id
  else id

globalUpdateHandler :: Event -> Game -> Game {- StateT Game IO () -}
globalUpdateHandler e@(EventKey key keyState _ _) = 
  if keyState == Down then
    case key of
      (Char 'S') -> id
      _ -> id
  else id
  
stepGame :: Float -> Game -> Game
stepGame _ = id

stepLevel :: Float -> Level Square -> Level Square
stepLevel _ = id