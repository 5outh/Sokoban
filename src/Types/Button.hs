module Types.Button(
  Rectangle(..),
  Button(..),
  renderButton,
  makeButton,
  restartButton,
  exitButton
) where

import Graphics.Gloss
import Data.Monoid((<>))

data Rectangle = Rectangle {ltbt :: Point, rttp :: Point}

data Button a = Button { rect :: Rectangle, 
                         name :: String,
                         func :: (Point -> a) }

makeButton :: String -> (Point -> a) -> Point -> Button a
makeButton s f (x, y) = Button rectangle s f
  where lth = 60 * (fromIntegral $ length s)
        height = 120
        rectangle = Rectangle (x, y) (x+lth, y+height)

rectToPicture :: Rectangle -> Picture
rectToPicture (Rectangle (lbx, lby) (rtx, rty)) = Polygon [(lbx, lby), (rtx, lby), (rtx, rty), (lbx, rty)]  
                         
renderButton :: Button a -> Picture
renderButton (Button rect@(Rectangle (x, y) _) txt _) = block <> txt'
  where block = Color black  $ rectToPicture rect
        txt'  = Color white $ Translate x (y+10) $ Text txt

inRect :: Point -> Rectangle -> Bool
inRect (x, y) (Rectangle (x1, y1) (x2, y2)) = and [x > x1, x < x2, y > y1, y < y2]

onClick :: Point -> Button a -> Maybe a
onClick p@(x, y) b = if inRect p (rect b) then Just ((func b) p) else Nothing

restartButton  = makeButton "Restart" id (0, 0)
exitButton     = makeButton "Exit"    id (0, 200)