module Types.Level(
  Level(..),
  levelToPicture,
  winningLevel
) where

import Types.Square
import Graphics.Gloss(Picture(..))

data Level a = Level
  { player :: a,
    boxes :: [a],
    walls :: [a],
    switches :: [a]
  } deriving (Show, Eq)
  
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