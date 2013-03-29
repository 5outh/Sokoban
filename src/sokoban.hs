{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import Engine.Update
import IO.LevelParser
import IO.Save
import Data.Char(toLower)
import System.Environment(getArgs)
import System.Exit

main = do
  args <- getArgs
  if null args then runGame
  else case args of
    (x:[]) -> case x of
      "reset" -> do 
                  putStrLn ("Are you sure you want to wipe your save file and start from the beginning? y/n")
                  ans <- getLine
                  checkRestart (toLower $ head ans)
      _       -> putStrLn ("Could not understand request " ++ x) >> exitFailure
    (a:b:_) -> case a of
      "sasquatch" -> do
        putStrLn "Are you sure you want to wipe your progress and load a new level pack? y/n"
        ans <- getLine
        checkSasquatch (toLower $ head ans) b
      "level" -> runGameFromLevel (readInt b)
      _           -> (putStrLn "Could not understand request") >> exitFailure

checkRestart :: Char -> IO ()
checkRestart c = case c of 
  'y' -> startGame >>= saveGame >> runGame
  'n' -> exitSuccess
  '_' -> do 
    putStrLn "could not understand request, please try another answer:"
    ans <- getLine
    checkRestart (toLower $ head ans)
              

checkSasquatch :: Char -> String -> IO ()
checkSasquatch c f = do
  case c of
    'y' -> writeSasquatch f >> runGame
    'n' -> exitSuccess
    _   -> do 
      putStrLn "could not understand request, please try another answer:"
      ans <- getLine
      checkSasquatch (toLower $ head ans) f