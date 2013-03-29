module Types.Button(
  Rectangle(..),
  Button(..),
  buttonToPicture,
  makeButton,
  restartButton,
  exitButton,
  inRect
) where

{- TODO: Fix button scaling click range 
         ATM buttons do not work        -}

import Graphics.Gloss
import Data.Monoid((<>))
import System.Exit(exitSuccess)

data Rectangle = Rectangle {ltbt :: Point, rttp :: Point}

data Button a = Button { rect :: Rectangle, 
                         name :: String}

makeButton :: String -> Point -> Button a
makeButton s (x, y) = Button rectangle s
  where lth = 60 * (fromIntegral $ length s)
        height = 120
        rectangle = Rectangle (x, y) (x+lth, y+height)

rectToPicture :: Rectangle -> Picture
rectToPicture (Rectangle (lbx, lby) (rtx, rty)) = Polygon [(lbx, lby), (rtx, lby), (rtx, rty), (lbx, rty)]  
                         
buttonToPicture :: Button a -> Picture
buttonToPicture (Button rect@(Rectangle (x, y) _) txt) = Scale 0.3 0.3 (block <> txt')
  where block = Color black  $ rectToPicture rect
        txt'  = Color white $ Translate x (y+10) $ Text txt

inRect :: Point -> Rectangle -> Bool
inRect (x, y) (Rectangle (x1, y1) (x2, y2)) = and [x > x1, x < x2, y > y1, y < y2]

restartButton  = makeButton "Restart" (800, 800)
exitButton     = makeButton "Quit"    (800, 600)