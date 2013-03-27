module World(
	levelToPicture,
	winningLevel,
  gameToPicture,
  nextLevel,
  winningGame
) where

import Graphics.Gloss(Picture(..))
import Types
import Square

instance Functor Level where
  fmap f (Level p bxs wls sws) = Level (f p) (map f bxs) (map f wls) (map f sws)

levelToPicture :: Level Square -> Picture
levelToPicture w@(Level p bxs wls sws) = 
  if not $ winningLevel w then
    Pictures $ map showSquare $ (wls ++ sws ++ bxs ++ [p])
  else Translate (-100) 0 $ Scale 0.5 0.5 $ Text "You Win!"

winningLevel :: Level Square -> Bool
winningLevel w = all (`elem` bxs) sws
  where (Level _ bxs _ sws) = fmap getPoint w
  
gameToPicture :: Game -> Picture
gameToPicture (Game _ lvl _) = levelToPicture lvl

nextLevel :: Game -> Int
nextLevel (Game i _ _) = succ i

winningGame :: Game -> Bool
winningGame (Game _ lvl _) = winningLevel lvl