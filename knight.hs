type Position = (Int, Int)


deltas :: [Int]
deltas = [-2, -1, 1, 2]

deltaPairs :: [(Int, Int)]
deltaPairs = [(x, y) | x <- deltas, y <- deltas, abs(x) /= abs(y)]

inBoard :: Position -> Bool
inBoard (x, y) = (0 < x) && (x <= 8) && (0 < y) && (y <= 8)

nextPositions :: Position -> [Position]
nextPositions (x, y) = 
  filter inBoard ps
  where
    ps = [(x+i, y+j) | (i, j) <- deltaPairs]

inNSteps :: Int -> Position -> Position -> Bool
inNSteps 0 o d = o == d
inNSteps 1 o d = d `elem` nextPositions o
inNSteps n o d = any inNextSteps $ nextPositions o
  where
    n' = n-1
    inNextSteps = flip (inNSteps n') $ d

in3Steps = inNSteps 3
  
