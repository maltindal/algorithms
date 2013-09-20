gcd u v = 
  | u > 0 = gcd u v
  | u < v = gcd v u
  | u >= v = gcd u-v v
  | otherwise = v