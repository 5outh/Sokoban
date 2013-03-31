module Types.Square(
	getSquare,
	showSquare,
	drawSquareAt,
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

showSquare :: Square -> IO Picture
showSquare square = case square of
                     (Wall p)         -> return $ Color black  $ drawSquareAt p 16
                     (Player p)       -> drawBMPAt p "./Images/Player.bmp"    
                     (Box p)          -> return $ Color violet $ drawSquareAt p 16
                     (Switch p)       -> return $ Color blue   $ drawSquareAt p 16   
                     (Floor p)        -> return $ Color white  $ drawSquareAt p 16

drawSquareAt :: Point -> Float -> Picture
drawSquareAt (x, y) scale = Translate (16*x) (16*y) $ rectangleSolid scale scale

drawBMPAt :: Point -> String -> IO Picture
drawBMPAt (x, y) path = do
  bmp <- loadBMP path
  return $ Translate (16*x) (16*y) $ bmp

getPoint :: Square -> Point
getPoint square = case square of
                    (Wall p)     -> p
                    (Player p)   -> p
                    (Switch p)   -> p                     
                    (Box p)      -> p
                    (Floor p)    -> p