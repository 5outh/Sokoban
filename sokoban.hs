import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (fromJust)
import Data.List(delete)
import Types

main = play 
  (InWindow "Sokoban" (800, 600) (400, 400))
  white
  45
  (parseLevel level)
  worldToPicture
  eventHandler
  stepWorld

eventHandler :: Event -> World Square -> World Square
eventHandler e@(EventKey key keyState mods coord) = 
  if keyState == Down then 
    case key of
                    (SpecialKey KeyUp)    -> updateBoard U
                    (SpecialKey KeyDown)  -> updateBoard D
                    (SpecialKey KeyLeft)  -> updateBoard L
                    (SpecialKey KeyRight) -> updateBoard R
                    _                     -> id
  else id

eventHandler e@(EventMotion coord) = id

stepWorld :: Float -> World Square -> World Square
stepWorld _ = id

worldToPicture :: World Square -> Picture
worldToPicture w@(World p bxs wls sws) = 
  Pictures $ map showSquare $ (wls ++ sws ++ bxs ++ [p])

level = unlines [
  "####"
  ,"# .#"
  ,"#  ###"
  ,"#*@  #"
  ,"#  $ #"
  ,"#  ###"
  ,"####"]

getSquare :: Char -> [Point -> Square]
getSquare c = case c of
                '#' -> [Wall]
                '@' -> [Player]
                '.' -> [Switch]
                '*' -> [Switch, Box]
                ' ' -> [Floor]
                '$' -> [Box]
                _   -> error "Invalid symbol on board"

getPoint square = case square of
                    (Wall p)     -> p
                    (Player p)   -> p
                    (Switch p)   -> p                     
                    (Box p)      -> p
                    (Floor p)    -> p

showSquare :: Square -> Picture
showSquare square = case square of
                     (Wall p)         -> Color black  $ drawSquare p
                     (Player p)       -> Color red    $ drawSquare p          
                     (Box p)          -> Color violet $ drawSquare p
                     (Switch p)       -> Color blue   $ drawSquare p   
                     (Floor p)        -> Color white  $ drawSquare p

drawSquare (x, y) = Translate (16*x) (16*y) $ rectangleSolid 16 16

winningBoard w = all (`elem` bxs) sws
  where (World _ bxs _ sws) = fmap getPoint w

moveBackwards :: Point -> Direction -> Point
moveBackwards p dir = movePoint p $ fromJust $ lookup dir opposites
  where opposites = zip [R,L,U,D] [L,R,D,U]

movePoint :: Point -> Direction -> Point
movePoint p@(x,y) dir =
    case dir of
      R -> (x+1, y)
      L -> (x-1, y)
      U -> (x, y+1)
      D -> (x, y-1)
      _ -> p

moveBox :: Direction -> Point -> World Square -> World Square
moveBox dir point world@(World plr bxs wls sws) = world'
  where boxPoint   = movePoint point dir
        (World plr' bxs' wls' sws') = fmap getPoint world
        world'
          | boxPoint `elem` bxs' = world
          | boxPoint `elem` wls' = world
          | boxPoint `elem` sws' = World 
                                   (Player point)
                                   (map Box (boxPoint: delete point bxs'))
                                   wls
                                   sws
          | otherwise            = World 
                                   (Player point)
                                   (map Box (boxPoint: delete point bxs'))
                                   wls
                                   sws

movePlayer :: Direction -> Point -> World Square -> World Square
movePlayer dir point w@(World _ boxes walls switches)
  | point `elem` bxs  = moveBox dir point w
  | point `elem` wls  = w
  | point `elem` sws  = World (Player point) boxes walls switches
  | otherwise         = World (Player point) boxes walls switches
  where wPoints@(World _ bxs wls sws) = fmap getPoint w

updateBoard :: Direction -> World Square -> World Square
updateBoard dir w = movePlayer dir p' w
  where p' = movePoint (getPoint $ player w) dir

parseLevel level = case (player initWorld) of
                        (Player (-1,-1)) -> error "No player on board"
                        (Player _      ) -> initWorld
                        _                -> error "Internal misbehavior -> Player is not what it seems."
  where lns = zip [1..] $ reverse $ lines level
        sqrs :: (Float, [Char]) -> [Square]
        sqrs (a, xs) = concatMap fix $ zip (map getSquare xs) ((zip [1..] (repeat a)) :: [Point])
                        where fix (xs, y) = zipWith ($) xs (repeat y)
        populateWorld [] w = w
        populateWorld (p:points) w@(World plr bxs wls sws) = populateWorld points w'
          where w' = case p of
                       p@(Player _)   -> World p bxs wls sws
                       b@(Box _ )     -> World plr (b:bxs) wls sws
                       w@(Wall _)     -> World plr bxs (w:wls) sws
                       s@(Switch _)   -> World plr bxs wls (s:sws)
                       _              -> w
        emptyWorld = World (Player (-1,-1)) [] [] []
        initWorld = populateWorld (lns >>= sqrs) emptyWorld