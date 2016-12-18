import Data.List (intercalate)
import System.Environment (getArgs)

main = do
  args <- getArgs
  let pArgs = progArgs args
  line <- getLine
  let blocks = readBlocks line
  let path = shortestPath blocks
  putStrLn $ outputResult pArgs path


outputResult :: Either String (ProgArgs Integer) -> Path Integer -> String
outputResult (Right (ProgArgs outputFormatter parallelNames)) p = outputFormatter parallelNames p
outputResult (Left message) _ = message


type ParallelNames = Parallel -> String
type OutputFormatter a = ParallelNames -> Path a -> String

data ProgArgs a = ProgArgs {
  outputFormater  :: OutputFormatter a,
  parallelNames :: ParallelNames
}

defaultProgArgs = ProgArgs instructPath cardinalNames


progArgs :: [String] -> Either String (ProgArgs Integer)
progArgs l = progArgs' defaultProgArgs l

progArgs' :: ProgArgs Integer -> [String] -> Either String (ProgArgs Integer)
progArgs' pa [] = Right pa
progArgs' (ProgArgs _ n) ("-i":as) = progArgs' (ProgArgs instructPath n) as
progArgs' (ProgArgs _ n) ("-r":as) = progArgs' (ProgArgs showRoads n) as
progArgs' (ProgArgs f _) ("-c":as) = progArgs' (ProgArgs f cardinalNames) as
progArgs' (ProgArgs f _) ("-l":as) = progArgs' (ProgArgs f letterNames) as
progArgs' _ (a:_) = Left $ "Invalid argument " ++ a


instructPath :: (Show a, Eq a, Num a) => ParallelNames -> Path a -> String
instructPath parallelName = intercalate "\n" . map (instructRoad parallelName) . fst

instructRoad :: (Show a, Eq a, Num a) => ParallelNames -> Road a -> String
instructRoad _ (Road _ 0) = "You arrived."
instructRoad parallelName (Road Cross l) = unwords instructionList
  where
    instructionList = ["Cross for",  show l, "miles."]

instructRoad parallelName (Road p l) = unwords instructionList
  where
    instructionList = ["Follow", parallelName p , "for", show l, "miles."]


showRoads :: (Show a, Eq a, Num a) => ParallelNames -> Path a -> String
showRoads parallelName = unwords . map parallelName . map parallel . fst



cardinalNames :: Parallel -> String
cardinalNames = show

letterNames :: Parallel -> String
letterNames North = "A"
letterNames South = "B"
letterNames Cross = "C"


-- HERE COMES THE INTERESTING PART

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

readBlocks :: String -> [Block Integer]
readBlocks = blocks . (map read) . words
