{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import Engine.Update
import IO.LevelParser
import IO.Save
import Data.Char(toLower)
import System.Environment(getArgs)

main = do
  args <- getArgs
  if null args then runGame
  else case (map toLower (head args)) of
    "sasquatch" -> writeSasquatch "../m1.txt"
    _           -> runGame