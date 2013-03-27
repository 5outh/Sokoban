module Save (
  loadGame,
  saveGame
)

where

import Types
import World
import LevelParser
import qualified Text.ParserCombinators.Parsec as P
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

getLevelNumber :: String -> Either P.ParseError Int
getLevelNumber = P.parse number "(unknown)"

number :: P.Parser Int
number = do
  P.string "level"
  P.many $ P.char '0'
  curLvl <- P.many $ P.oneOf "123456789"
  return $ readInt curLvl

readInt :: String -> Int
readInt x = read x :: Int