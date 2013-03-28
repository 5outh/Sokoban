module IO.Save (
  loadGame,
  saveGame
)

where

import Types.Types
import Types.World
import IO.LevelParser
import Text.ParserCombinators.Parsec
import System.Directory(doesFileExist)

startGame = do
  lvl <- readFile "levels/level001.lvl"
  let lvl' = parseLevel lvl
  return $ Game 1 lvl' False

saveGame :: Game -> IO ()
saveGame = writeFile "savegame.sav" . gameToFileName

loadGame :: IO Game
loadGame = doesFileExist "savegame.sav" >>= \exists -> 
  if exists then do
    levelName <- readFile "savegame.sav"
    lvl       <- readFile levelName
    let n = case getLevelNumber levelName of
              Right x -> x
              Left  e -> error "Failure to read save file."
    return $ Game n (parseLevel lvl) False
  else startGame

getLevelNumber :: String -> Either ParseError Int
getLevelNumber = parse number "(unknown)"

number :: Parser Int
number = do
  string "level"
  many $ char '0'
  curLvl <- many $ oneOf "123456789"
  return $ readInt curLvl

readInt :: String -> Int
readInt x = read x :: Int