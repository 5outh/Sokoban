module Square(
	getSquare,
	showSquare,
	drawSquare,
	getPoint
) where
	
import Graphics.Gloss
import Types

getSquare :: Char -> [Point -> Square]
getSquare c = case c of
                '#' -> [Wall]
                '@' -> [Player]
                '.' -> [Switch]
                '*' -> [Switch, Box]
                ' ' -> [Floor]
                '$' -> [Box]
                _   -> error "Invalid symbol on board"

showSquare :: Square -> Picture
showSquare square = case square of
                     (Wall p)         -> Color black  $ drawSquare p
                     (Player p)       -> Color red    $ drawSquare p          
                     (Box p)          -> Color violet $ drawSquare p
                     (Switch p)       -> Color blue   $ drawSquare p   
                     (Floor p)        -> Color white  $ drawSquare p

drawSquare (x, y) = Translate (16*x) (16*y) $ rectangleSolid 16 16

getPoint square = case square of
                    (Wall p)     -> p
                    (Player p)   -> p
                    (Switch p)   -> p                     
                    (Box p)      -> p
                    (Floor p)    -> p