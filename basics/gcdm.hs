gcdm u v
  | u == 0 = v
  | u < v = gcdm v u
  | u >= v = gcdm (u `mod` v) v