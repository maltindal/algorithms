-- This sieve uses mod. It's not the sieve of eratosthenes.
sievem [] = []
sievem (p:z) = p : sievem [x | x <- z, x `mod` p > 0]

-- The real sieve of eratosthenes only uses the plus (+) operation :-)
-- assumes that the list of numbers ns starts with the digit 2
sieve n m ns 
  | m <= (length ns + 2) = sieve n (m+n) (map (map_fn) ns)
  | m > (length ns + 2) && n < (length ns + 2) = sieve (n+1) (n+n+2) (map (map_fn) ns)
  | otherwise = ns
  where map_fn x
          | x==m = 0
          | otherwise = x

prims [] = []
prims (x:xs) = filter (/=0) (sieve x (x+x) (x:xs))

-- calculates the distance between two pairs of primes
-- expects a list of prime numbers
distance ps = [ y-x | (x,y) <- zip ps (tail ps) ]

-- given a list of numbers returns the max number
max' xs = foldl (max ) (head xs) (tail xs)

-- given a sequence of numbers calculate the max distance
-- between two pairs of prime numbers 2..r
maxdist rs = [max' (distance (sievem [2..x])) | x <- rs]

-- given a number check if it is prime
isPrime x = null [y | y<-[2..floor (sqrt (fromIntegral x))], x `mod` y == 0]