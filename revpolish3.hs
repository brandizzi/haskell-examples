main = do
  c <- getContents
  putStrLn $ present c

stack "+" (x:x':xs) = (x+x'):xs
stack "-" (x:x':xs) = (x-x'):xs
stack "*" (x:x':xs) = (x*x'):xs
stack "/" (x:x':xs) = (x `quot` x'):xs
stack s xs = (read s :: Integer):xs

collapse [] = []
collapse (x:xs) = stack x (collapse xs)

parse = reverse . words
interpret = collapse . parse
present = unwords . map show . interpret
