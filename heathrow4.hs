import Control.Monad (when)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (die)
import Heathrow

main = do
  mArgs <- progArgs <$> getArgs

  when (not $ validArgs mArgs) $ do
    let (Left message) = mArgs
    die message

  let (Right args) = mArgs
  line <- getLine
  let blocks = readBlocks line
  let path = shortestPath blocks
  putStrLn $ outputResult args path

validArgs :: Either String (ProgArgs a) -> Bool
validArgs (Right _) = True
validArgs (Left _) = False

outputResult :: ProgArgs Integer -> Path Integer -> String
outputResult (ProgArgs outputFormatter parallelNames) p = outputFormatter parallelNames p

type ParallelNames = Parallel -> String
type OutputFormatter a = ParallelNames -> Path a -> String

data ProgArgs a = ProgArgs {
  outputFormater  :: OutputFormatter a,
  parallelNames :: ParallelNames
}

defaultProgArgs = ProgArgs instructPath cardinalNames


progArgs :: [String] -> Either String  (ProgArgs Integer)
progArgs = progArgs' defaultProgArgs

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


