import Types.Button
import Graphics.Gloss.Interface.IO.Animate
import Data.Monoid((<>))

render _ = loadBMP "./Images/Player.bmp"

main = animateIO
  (InWindow "Test" (800, 600) (400, 400))
  white
  render