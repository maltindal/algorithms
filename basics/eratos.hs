import System.Random

-- Sieve implementations
------------------------

-- This sieve uses mod. It's not the sieve of eratosthenes.
sievem :: Integral a => [a] -> [a]
sievem [] = []
sievem (p:z) = p : sievem [x | x <- z, x `mod` p > 0]

-- The real sieve of eratosthenes only uses the plus (+) operation :-)
-- assumes that the list of numbers ns starts with the digit 2
sieve :: Int -> Int -> [Int] -> [Int]
sieve n m ns 
  | m <= (length ns + 2) = sieve n (m+n) (map (map_fn) ns)
  | m > (length ns + 2) && n < (length ns + 2) = sieve (n+1) (n+n+2) (map (map_fn) ns)
  | otherwise = ns
  where map_fn x
          | x==m = 0
          | otherwise = x

prims :: [Int] -> [Int]
prims [] = []
prims (x:xs) = filter (/=0) (sieve x (x+x) (x:xs))

-- given a list of prime numbers
-- calculate the distance between two pairs of primes
distance :: [Int] -> [Int]
distance ps = [ y-x | (x,y) <- zip ps (tail ps) ]

-- given a list of numbers returns the max number
max' :: [Int] -> Int
max' xs = foldl (max ) (head xs) (tail xs)

-- given a sequence of numbers calculate the max distance
-- between two pairs of prime numbers 2..r
maxdist :: [Int] -> [Int]
maxdist rs = [max' (distance (sievem [2..x])) | x <- rs]


-- Primality tests
------------------

-- given a number n check if it is prime
-- using the trial division method
trial_div :: Int -> Bool
trial_div n = null [y | y<-[2..floor (sqrt (fromIntegral n))], n `mod` y == 0]

-- given a number n check if it is a fermat pseudoprime
fermat :: Int -> Bool
fermat n = foldl (&&) True [ fermat_theorem n a | a<-(rnd (n-1)) ]
  where
    fermat_theorem :: Int -> Int -> Bool
    fermat_theorem n a = mod ((fromIntegral a) ^ (n-1)) (fromIntegral n) == 1

    rnd :: Int -> [Int]
    rnd x = take (if (x-1)>5 then 5 else (x-1)) (randomRs (1,x) g)
      where
        g = mkStdGen 42 -- todo: implement better pseudo randomness

-- given a number n check its primality using
-- the rabin-miller test (todo)

-- given a number n check its primality using
-- the lucas lehmer test (todo)
