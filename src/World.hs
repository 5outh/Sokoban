module World(
	worldToPicture,
	winningWorld,
	stepWorld
) where

import Graphics.Gloss(Picture(..))
import Types
import Square

instance Functor World where
  fmap f (World p bxs wls sws) = World (f p) (map f bxs) (map f wls) (map f sws)

worldToPicture :: World Square -> Picture
worldToPicture w@(World p bxs wls sws) = 
  Pictures $ map showSquare $ (wls ++ sws ++ bxs ++ [p])

winningWorld :: World Square -> Bool
winningWorld w = all (`elem` bxs) sws
  where (World _ bxs _ sws) = fmap getPoint w

stepWorld :: Float -> World Square -> World Square
stepWorld _ = id