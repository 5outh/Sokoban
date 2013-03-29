module IO.Save (
  loadGame,
  saveGame,
  startGame,
  goToNextLevelAndSave
) where

import Types.Game
import IO.LevelParser
import Text.ParserCombinators.Parsec
import System.Directory(doesFileExist)

startGame = do
  lvl <- readFile "levels/level001.lvl"
  let lvl' = parseLevel lvl
  return $ Game 1 lvl' False

saveGame :: Game -> IO ()
saveGame = writeFile "savegame.sav" . show . levelNumber

loadGame :: IO Game
loadGame = doesFileExist "savegame.sav" >>= \exists -> 
  if exists then do
    levelName <- readFile "savegame.sav"
    lvl       <- readFile $ intToFileName (readInt levelName)
    let n = readInt levelName
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

goToNextLevelAndSave :: Game -> IO Game
goToNextLevelAndSave g@(Game i _ _) = do
  let next = goToNextLevel g
  saveGame next
  g' <- loadGame
  return g'


intToFileName :: Int -> String
intToFileName i = "levels/level" ++ (toThousand i) ++ ".lvl"