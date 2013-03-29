import Types.Button
import Graphics.Gloss.Interface.Pure.Animate
import Data.Monoid((<>))

button  = makeButton "Restart" id (0, 0) 
button' = makeButton "Exit"    id (0, 200)

render _ = renderButton button <> renderButton button'

summat _ = Circle 10

main = animate 
  (InWindow "Test" (800, 600) (400, 400))
  white
  render