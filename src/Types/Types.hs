module Types.Types(
  Square(..),
  Level(..),
  Direction(..),
  Game(..)
) where

import Graphics.Gloss

data Game = Game
  { levelNumber  :: Int,
    currentLevel :: Level Square,
    won          :: Bool }

data Square = Player Point
            | Box Point 
            | Wall Point 
            | Switch Point 
            | Floor Point
            deriving (Show, Eq)
            
data Level a = Level
  { player :: a,
    boxes :: [a],
    walls :: [a],
    switches :: [a]
  } deriving (Show, Eq)
  
data Direction = L | R | U | D | Other deriving (Show, Eq)