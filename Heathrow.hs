module Heathrow (
  Parallel (North, South, Cross),
  Road (Road),
  Block (Block),
  Path,
  roadLength,
  parallel,
  readBlocks,
  shortestPath
) where

data Parallel = North | South | Cross deriving Show

other :: Parallel -> Parallel
other North = South
other South = North


data Road a = Road {
  parallel :: Parallel,
  roadLength   :: a
} deriving Show

data Block a = Block {
  northRoad :: Road a,
  southRoad :: Road a,
  crossRoad :: Road a
} deriving Show

block :: a -> a -> a -> Block a
block a1 a2 a3 = Block (Road North a1) (Road South a2) (Road Cross a3)

blocks :: [a] -> [Block a]
blocks (a1:a2:a3:as) = (block a1 a2 a3):(blocks as)
blocks _ = []

readBlocks :: String -> [Block Integer]
readBlocks = blocks . (map read) . words


type Path a = ([Road a], a)

path :: (Num a) => [Road a] -> Path a
path rs = (rs, sum $ map roadLength rs)


minPath :: (Ord a, Num a) => Path a -> Path a -> Path a
minPath p1@(roads1, length1) p2@(roads2, length2) =
  if length1 < length2 then p1 else p2

(+++) :: (Num a) => Path a -> Path a -> Path a

(+++) (roads1, length1) (roads2, length2) =
  (roads1 ++ roads2, length1 + length2)
     

pathsBy :: (Num a) => Parallel -> Block a -> (Path a, Path a)

pathsBy North (Block nr sr cr) = (path [nr], path [nr, cr])

pathsBy South (Block nr sr cr) = (path [sr], path [sr, cr])


shortestPathBy :: (Ord a, Num a) => Parallel -> [Block a] -> Path a

shortestPathBy p (b:[]) = minPath straight crossed
  where
    (straight, crossed) = pathsBy p b

shortestPathBy p (b:bs) = minPath byStraight byCrossed
  where
    (straight, crossed) = pathsBy p b
    followingStraight   = shortestPathBy p bs
    followingCrossed    = shortestPathBy (other p) bs
    byStraight          = straight +++ followingStraight
    byCrossed           = crossed +++ followingCrossed

shortestPath :: (Ord a, Num a) => [Block a] -> Path a
shortestPath bs = minPath (shortestPathBy North bs) (shortestPathBy South bs)
