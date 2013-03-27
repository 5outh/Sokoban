module LevelParser(
	parseLevel,
  writeSasquatch
)where

import Graphics.Gloss(Point)
import Types
import World
import Square
import Text.ParserCombinators.Parsec
import Text.Parsec

parseLevel :: String -> World Square
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

toThousand :: Int -> String
toThousand x
  | x < 10   = "00" ++ show x
  | x < 100  = "0" ++ show x
  | x < 1000 = show x
  | otherwise = error "number is greater than 1000"
        
writeSasquatch :: FilePath -> IO ()
writeSasquatch file = do
  contents <- readFile file
  let levels = case parseSasquatch contents of
                  Right a -> zip (map 
                             (\x -> "levels/level" ++ toThousand x ++ ".lvl") [1..]) 
                             a
                  Left _  -> error "Parse error"
      doWrite (a, b) = writeFile a b
  mapM_ doWrite levels
  return ()

parseSasquatch :: String -> Either ParseError [String]
parseSasquatch = parse (many level) "(unknown)"

level :: Parser String
level = do
  spaces
  char ';'
  space
  many $ oneOf ['0'..'9']
  optional space
  optional title
  space
  contents <- many $ noneOf ";"
  return contents

title :: Parser ()
title = do
  char '\''
  many $ noneOf "\'"
  char '\''
  return ()