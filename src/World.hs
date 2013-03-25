module World(
	worldToPicture,
	winningWorld
) where

import Graphics.Gloss(Picture(..))
import Types
import Square

instance Functor World where
  fmap f (World p bxs wls sws) = World (f p) (map f bxs) (map f wls) (map f sws)

worldToPicture :: World Square -> Picture
worldToPicture w@(World p bxs wls sws) = 
  if not $ winningWorld w then
    Pictures $ map showSquare $ (wls ++ sws ++ bxs ++ [p])
  else Translate (-100) 0 $ Scale 0.5 0.5 $ Text "You Win!"

winningWorld :: World Square -> Bool
winningWorld w = all (`elem` bxs) sws
  where (World _ bxs _ sws) = fmap getPoint w