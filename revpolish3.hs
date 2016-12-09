main = do
  c <- getContents
  putStrLn $ unwords . map show . interpret' $ c

stack :: (Floating a, Read a) => String -> [a] -> [a]
stack "+" (x:x':xs) = (x+x'):xs
stack "-" (x:x':xs) = (x'-x):xs
stack "*" (x:x':xs) = (x*x'):xs
stack "/" (x:x':xs) = (x'/ x):xs
stack "ln" (x:xs) = (log x):xs
stack s xs = (read s):xs

collapse [] = []
collapse (x:xs) = stack x (collapse xs)

collapse' = foldl (flip stack) []


parse = reverse . words
interpret = collapse . parse
interpret' = collapse' . words
present = unwords . map show . interpret
