import Types.Game
import IO.Save
import Control.Monad.State

nextLvl (Game i lvl w) = Game (succ i) lvl w

updateGame :: StateT Game IO ()
updateGame = do
  game <- get
  lift $ saveGame game
  lift $ putStrLn "Wat"
  
process :: [Char] -> StateT Game IO ()
process cs = forM_ cs $ \c -> case c of
  '+' -> updateGame
  '-' -> modify id
  _   -> modify id

main = do
  game <- startGame
  runStateT (process "+-+") game