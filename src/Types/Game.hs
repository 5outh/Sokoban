module Types.Game(
  Game(..),
  gameToPicture,
  nextLevel,
  winningGame,
  gameToFileName,
  toThousand,
  goToNextLevel
) where

import Types.Square
import Types.Level
import Graphics.Gloss(Picture(..))
import Data.Monoid

data Game = Game
  { levelNumber  :: Int,
    currentLevel :: Level Square,
    won          :: Bool }
    
gameToPicture :: Game -> IO Picture
gameToPicture (Game i lvl w) = 
  if not w then return $ displayLevel `mappend` levelToPicture lvl
  else return $ Translate (-100) 0 $ Scale 0.5 0.5 $ Text "You Win!"
    where displayLevel = Scale 0.5 0.5 $ Translate (-700) (450) $ Text $  "Level " ++ (show i)

nextLevel :: Game -> Int
nextLevel (Game i _ _) = succ i

winningGame :: Game -> Bool
winningGame (Game _ lvl _) = winningLevel lvl

gameToFileName :: Game -> String
gameToFileName (Game i _ _) = "levels/level" ++ (toThousand i) ++ ".lvl"

toThousand :: Int -> String
toThousand x
  | x < 10   = "00" ++ show x
  | x < 100  = "0" ++ show x
  | x < 1000 = show x
  | otherwise = error "number is greater than 1000"
  
goToNextLevel :: Game -> Game
goToNextLevel (Game i lvl w) = Game (succ i) lvl w