gcd_ u v
  | u == 0 = v
  | u < v = gcd_ v u
  | u >= v = gcd_ (u-v) v
             
gcdm u v
  | u == 0 = v
  | u < v = gcdm v u
  | u >= v = gcdm (u `mod` v) v