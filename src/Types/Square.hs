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
                     (Wall p)         -> Color black  $ drawSquare p
                     (Player p)       -> Color red    $ drawSquare p          
                     (Box p)          -> Color violet $ drawSquare p
                     (Switch p)       -> Color blue   $ drawSquare p   
                     (Floor p)        -> Color white  $ drawSquare p

drawSquare :: (Float, Float) -> Picture
drawSquare (x, y) = Translate (16*x) (16*y) $ rectangleSolid 16 16

getPoint :: Square -> Point
getPoint square = case square of
                    (Wall p)     -> p
                    (Player p)   -> p
                    (Switch p)   -> p                     
                    (Box p)      -> p
                    (Floor p)    -> p