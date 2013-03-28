module Types.Misc(
  Direction(..),
) where
  
data Direction = L | R | U | D | Other deriving (Show, Eq)