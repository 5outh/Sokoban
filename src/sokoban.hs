import qualified Update as U
import LevelParser
import Data.Char(toLower)
import System.Environment(getArgs)

main = do
  args <- getArgs
  if null args then U.runGame
  else case (map toLower (head args)) of
    "sasquatch" -> writeSasquatch "../m1.txt"
    _           -> U.runGame