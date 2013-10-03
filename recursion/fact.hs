-- given a number n calculate it's factorial
-- where n is greater or equal to 0.
fact :: (Eq a, Num a) => a -> a
fact n = if n==0 then 1 else n * fact (n-1)