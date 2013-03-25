import qualified Update as U
import LevelParser
import Data.Char(toLower)
import System.Environment(getArgs)

main = do
  args@(a:_) <- getArgs
  if null args then U.runGame
  else case (map toLower a) of
    "sasquatch" -> writeSasquatch "../s1.txt"
    _           -> U.runGame