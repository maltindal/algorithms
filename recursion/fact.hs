fact n 
  | n > 0 = foldl (*) 1 [1..n]
  | otherwise = 1