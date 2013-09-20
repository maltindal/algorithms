sieve [] = []
sieve (p:z) = p : sieve [x | x <- z, x `mod` p > 0]