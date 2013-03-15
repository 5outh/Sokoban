module Types(
  Square(..),
  World(..),
  Direction(..)
) where

import Graphics.Gloss

data Square = Player Point
            | Box Point 
            | Wall Point 
            | Switch Point 
            | Floor Point
            deriving (Show, Eq)
            
data World a = World
  { player :: a,
    boxes :: [a],
    walls :: [a],
    switches :: [a]
  } deriving (Show, Eq)

instance Functor World where
  fmap f (World p bxs wls sws) = World (f p) (map f bxs) (map f wls) (map f sws)
  
data Direction = L | R | U | D | Other deriving (Show, Eq)