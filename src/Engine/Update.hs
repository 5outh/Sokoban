module Engine.Update(
	runGame,
	runGameFromLevel,
	moveBackwards,
	moveBox,
	movePlayer,
	updateBoard,
	gameEventHandler,
	stepLevel,
  stepGame
) where

import Graphics.Gloss(Point)
import Graphics.Gloss.Interface.IO.Game
import Types.Misc
import Types.Square
import Types.Game
import Types.Button
import Types.Level
import IO.Save
import Data.Maybe (fromJust)
import Data.List(delete)
import IO.LevelParser
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Exit(exitSuccess)

runGame = loadGame >>= \game ->
  playIO 
    (InWindow "Sokoban" (800, 600) (400, 400)) -- Create Window
    white                                      -- BG Color
    45                                         -- FPS
    game                                       -- Init. Game (:: Game)
    gameToPicture                              -- Draw Game (:: Game -> IO Picture)
    gameEventHandler                           -- Update Game (:: Event -> Game -> IO Game)
    stepGame                                   -- Step Game (:: Float -> Game -> IO Game)

runGameFromLevel n = startGameAtLevel n >>= \game -> 
  playIO 
    (InWindow "Sokoban" (800, 600) (400, 400)) -- Create Window
    white                                      -- BG Color
    45                                         -- FPS
    game                                       -- Init. Game (:: Game)
    gameToPicture                              -- Draw Game (:: Game -> IO Picture)
    gameEventHandler                           -- Update Game (:: Event -> Game -> IO Game)
    stepGameWithoutSave                        -- Step Game (:: Float -> Game -> IO Game)

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

gameEventHandler :: Event -> Game -> IO Game
gameEventHandler e@(EventMotion _) g = return g
gameEventHandler e@(EventKey key _ _ _) g@(Game i lvl won) = case key of
  (SpecialKey _) -> return $ Game i (movementHandler e lvl) won
  _              -> globalUpdateHandler e g
gameEventHandler _ g = return g
  
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

globalUpdateHandler :: Event -> Game -> IO Game 
globalUpdateHandler e@(EventKey key keyState _ p) g = 
  if keyState == Down then
    case key of
      (Char 'S') -> do
        saveGame g
        return g
      (Char 'r') -> loadGame
      (MouseButton LeftButton) -> if      p `inRect` (rect restartButton) then loadGame
                                  else if p `inRect` (rect exitButton)    then exitSuccess
                                  else return g
      _ -> return g
  else return g
  
stepGame :: Float -> Game -> IO Game
stepGame f g@(Game i lvl w) = if winningGame g then goToNextLevelAndSave g else return g

stepGameWithoutSave :: Float -> Game -> IO Game
stepGameWithoutSave f g | winningGame g = goToNextLevelWithoutSaving g
                        | otherwise     = return g

stepLevel :: Float -> Level Square -> Level Square
stepLevel _ = id