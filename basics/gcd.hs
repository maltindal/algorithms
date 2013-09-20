gcd_ u v
  | u == 0 = v
  | u < v = gcd_ v u
  | u >= v = gcd_ (u-v) v