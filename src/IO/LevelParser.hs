module IO.LevelParser(
	parseLevel,
  writeSasquatch,
  toThousand
)where

import Graphics.Gloss(Point)
import Types.Misc
import Types.Square
import Text.ParserCombinators.Parsec
import Text.Parsec
import Types.Level
import Types.Game(toThousand)

parseLevel :: String -> Level Square
parseLevel level = case (player initWorld) of
                        (Player (-1,-1)) -> error "No player on board"
                        (Player _      ) -> initWorld
                        _                -> error "Internal misbehavior -> Player is not what it seems."
  where lns = zip [1..] $ reverse $ lines level
        sqrs :: (Float, [Char]) -> [Square]
        sqrs (a, xs) = concatMap fix $ zip (map getSquare xs) ((zip [1..] (repeat a)) :: [Point])
                        where fix (xs, y) = zipWith ($) xs (repeat y)
        populateWorld [] w = w
        populateWorld (p:points) w@(Level plr bxs wls sws) = populateWorld points w'
          where w' = case p of
                       p@(Player _)   -> Level p bxs wls sws
                       b@(Box _ )     -> Level plr (b:bxs) wls sws
                       w@(Wall _)     -> Level plr bxs (w:wls) sws
                       s@(Switch _)   -> Level plr bxs wls (s:sws)
                       _              -> w
        emptyWorld = Level (Player (-1,-1)) [] [] []
        initWorld = populateWorld (lns >>= sqrs) emptyWorld
        
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