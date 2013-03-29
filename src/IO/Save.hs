module IO.Save (
  loadGame,
  saveGame,
  startGame,
  startGameAtLevel,
  goToNextLevelAndSave,
  goToNextLevelWithoutSaving,
  writeSasquatch,
  readInt,
) where

import Types.Game
import IO.LevelParser
import Text.ParserCombinators.Parsec
import System.Directory(doesFileExist)
import System.Exit(exitFailure)

startGame :: IO Game
startGame = startGameAtLevel 1

startGameAtLevel :: Int -> IO Game
startGameAtLevel i = doesFileExist (intToFileName i) >>= \exists ->
  if exists then do
    lvl <- readFile (intToFileName i)
    let lvl' = parseLevel lvl
    return $ Game i lvl' False 
  else putStrLn ("Level " ++ show i ++ " does not exist.") >> exitFailure

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
goToNextLevelAndSave g = do
  let next = goToNextLevel g
  saveGame next
  g' <- loadGame
  return g'

goToNextLevelWithoutSaving :: Game -> IO Game
goToNextLevelWithoutSaving g@(Game i _ _) = do
  let nextFile = intToFileName (succ i)
  nextLevel <- readFile nextFile
  let lvl = parseLevel nextLevel
  return $ Game (succ i) lvl False

intToFileName :: Int -> String
intToFileName i = "levels/level" ++ (toThousand i) ++ ".lvl"

writeSasquatch :: FilePath -> IO ()
writeSasquatch file = do
  contents <- readFile file
  let levels = case parseSasquatch contents of
                  Right a -> zip (map 
                             (\x -> "levels/level" ++ toThousand x ++ ".lvl") [1..]) 
                             a
                  Left _  -> error "Parse error"
  mapM_ (uncurry writeFile) levels
  startGame >>= saveGame
  return ()