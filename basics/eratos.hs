import System.Random

-- Sieve implementations
------------------------

-- This sieve uses mod. It's not the sieve of eratosthenes.
sievem :: Integral a => [a] -> [a]
sievem [] = []
sievem (p:z) = p : sievem [x | x <- z, mod x p > 0]

-- The real sieve of eratosthenes only uses the plus (+) operation :-)
-- assumes that the list of numbers ns starts with the digit 2
sieve :: Int -> Int -> [Int] -> [Int]
sieve n m ns 
  | m <= (length ns + 2) = sieve n (m+n) (map map_fn ns)
  | m > (length ns + 2) && n < (length ns + 2) = sieve (n+1) (n+n+2) (map map_fn ns)
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
max' xs = foldl max (head xs) (tail xs)

-- given a sequence of numbers calculate the max distance
-- between two pairs of prime numbers 2..r
maxdist :: [Int] -> [Int]
maxdist rs = [max' (distance (sievem [2..x])) | x <- rs]


-- Primality tests
------------------

-- given a number n check if it is prime
-- using the trial division method
trialDiv :: Int -> Bool
trialDiv n = null [y | y<-[2..floor (sqrt (fromIntegral n))], mod n y == 0]

-- given a number n check if it is a fermat pseudoprime
fermat :: Int -> Bool
fermat n = and [fermat_theorem n a | a <- rnd 1 (n - 1)]
  where fermat_theorem :: Int -> Int -> Bool
        fermat_theorem n a = mod (fromIntegral a ^ (n - 1)) (fromIntegral n) == 1

-- given the limits a and b of a range [a..b]
-- compute a random number in this range
rnd :: Int -> Int -> [Int]
rnd a b = take (if (b - 1) > 5 then 5 else b - 1) (randomRs (a, b) g)
  where g = mkStdGen 42 -- todo: implement better pseudo randomness

-- given a odd number n > 3 check its primality using the rabin-miller test
-- see Miller, Gary L. (1976),
--     "Riemann's Hypothesis and Tests for Primality",
--     Journal of Computer and System Sciences 13 (3): 300–317
rabinMiller :: Int -> Bool
rabinMiller n = and [ chk a | a <- rnd 2 (n - 1)]
    where
      s = ceiling (logBase 2 (fromIntegral n - 1))
      d = div (n - 1) 2

      chk :: Int -> Bool
      chk a = let x = mod (a ^ d) n
              in (x == 1 || x == (n - 1)) || chkNext x 1
      
      chkNext :: (Integral a) => a -> Int -> Bool
      chkNext x i
        | i < (s - 1) = let x = mod (x ^ 2) n
                        in x /= 1
        | otherwise = chkNext x (i + 1)
