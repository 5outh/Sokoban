module LevelParser(
	parseLevel
)where

import Graphics.Gloss(Point)
import Types
import World
import Square

parseLevel level = case (player initWorld) of
                        (Player (-1,-1)) -> error "No player on board"
                        (Player _      ) -> initWorld
                        _                -> error "Internal misbehavior -> Player is not what it seems."
  where lns = zip [1..] $ reverse $ lines level
        sqrs :: (Float, [Char]) -> [Square]
        sqrs (a, xs) = concatMap fix $ zip (map getSquare xs) ((zip [1..] (repeat a)) :: [Point])
                        where fix (xs, y) = zipWith ($) xs (repeat y)
        populateWorld [] w = w
        populateWorld (p:points) w@(World plr bxs wls sws) = populateWorld points w'
          where w' = case p of
                       p@(Player _)   -> World p bxs wls sws
                       b@(Box _ )     -> World plr (b:bxs) wls sws
                       w@(Wall _)     -> World plr bxs (w:wls) sws
                       s@(Switch _)   -> World plr bxs wls (s:sws)
                       _              -> w
        emptyWorld = World (Player (-1,-1)) [] [] []
        initWorld = populateWorld (lns >>= sqrs) emptyWorld