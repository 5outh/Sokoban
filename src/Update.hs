module Update(
	runGame,
	moveBackwards,
	moveBox,
	movePlayer,
	updateBoard,
	eventHandler
) where

import Graphics.Gloss(Point)
import Graphics.Gloss.Interface.Pure.Game
import World
import Types
import Square
import Data.Maybe (fromJust)
import Data.List(delete)
import LevelParser


runGame = play 
  (InWindow "Sokoban" (800, 600) (400, 400))
  white
  45
  (parseLevel level)
  worldToPicture
  eventHandler
  stepWorld

level = unlines [
  "####"
  ,"# .#"
  ,"#  ###"
  ,"#*@  #"
  ,"#  $ #"
  ,"#  ###"
  ,"####"]

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

moveBox :: Direction -> Point -> World Square -> World Square
moveBox dir point world@(World plr bxs wls sws) = world'
  where boxPoint   = movePoint point dir
        (World plr' bxs' wls' sws') = fmap getPoint world
        world'
          | boxPoint `elem` bxs' = world
          | boxPoint `elem` wls' = world
          | boxPoint `elem` sws' = World 
                                   (Player point)
                                   (map Box (boxPoint: delete point bxs'))
                                   wls
                                   sws
          | otherwise            = World 
                                   (Player point)
                                   (map Box (boxPoint: delete point bxs'))
                                   wls
                                   sws

movePlayer :: Direction -> Point -> World Square -> World Square
movePlayer dir point w@(World _ boxes walls switches)
  | point `elem` bxs  = moveBox dir point w
  | point `elem` wls  = w
  | point `elem` sws  = World (Player point) boxes walls switches
  | otherwise         = World (Player point) boxes walls switches
  where wPoints@(World _ bxs wls sws) = fmap getPoint w


updateBoard :: Direction -> World Square -> World Square
updateBoard dir w = movePlayer dir p' w
  where p' = movePoint (getPoint $ player w) dir

eventHandler :: Event -> World Square -> World Square
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