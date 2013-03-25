module LevelParser(
	parseLevel,
  writeSasquatch
)where

import Graphics.Gloss(Point)
import Types
import World
import Square
import Text.ParserCombinators.Parsec

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

parseLevelFromFile f = do
	contents <- readFile f
	return contents

--parseLevel' :: String -> Either ParseError (World Square)
parseLevel' level = parse square "(unknown)" level

square :: Parser [Square]
square = do
	s <- oneOf "@#*. $" <|> error "unknown type"
	return $ fix (getSquare s) (1, 1)
		where fix xs y = zipWith ($) xs (repeat y)
	
testParse = parseLevel' "@#. $$"

writeSasquatch :: FilePath -> IO ()
writeSasquatch file = do
  contents <- readFile file
  let levels = case parseSasquatch contents of
                  Right x -> zip (map (\x -> "levels/level" ++ toThousand x ++ ".lvl") [1..]) x
                  Left _  -> error "Parse error"
      doWrite (a, b) = writeFile a b
  mapM_ (doWrite) levels
  putStrLn contents

toThousand :: Int -> String
toThousand x
  | x < 10   = "00" ++ show x
  | x < 100  = "0" ++ show x
  | x < 1000 = show x
  | otherwise = error "number is greater than 1000"

parseSasquatch :: String -> Either ParseError [String]
parseSasquatch = parse sasquatch "(unknown)"

--ignore whitespace, break on ;, consume space and int
sasquatch :: Parser [String]
sasquatch = do
  levels <- many level
  return levels
  
level :: Parser String
level = do
  spaces
  char ';'
  space
  anyChar
  contents <- many $ noneOf ";"
  return contents