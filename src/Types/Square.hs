module Types.Square(
	getSquare,
	showSquare,
	drawSquare,
	getPoint,
  Square(..)
) where
	
import Graphics.Gloss

data Square = Player Point
            | Box Point 
            | Wall Point 
            | Switch Point 
            | Floor Point
            deriving (Show, Eq)

getSquare :: Char -> [Point -> Square]
getSquare c = case c of
                '#' -> [Wall]
                '@' -> [Player]
                '.' -> [Switch]
                '*' -> [Switch, Box]
                ' ' -> [Floor]
                '$' -> [Box]
                _   -> []

showSquare :: Square -> Picture
showSquare square = case square of
                     (Wall p)         -> Color black  $ drawSquare p 16
                     (Player p)       -> Color red    $ drawSquare p 16      
                     (Box p)          -> Color violet $ drawSquare p 16
                     (Switch p)       -> Color blue   $ drawSquare p 16   
                     (Floor p)        -> Color white  $ drawSquare p 16

drawSquare :: (Float, Float) -> Float -> Picture
drawSquare (x, y) scale = Translate (16*x) (16*y) $ rectangleSolid scale scale

getPoint :: Square -> Point
getPoint square = case square of
                    (Wall p)     -> p
                    (Player p)   -> p
                    (Switch p)   -> p                     
                    (Box p)      -> p
                    (Floor p)    -> p