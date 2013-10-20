import Data.Char

-- ciphers
----------

-- caesar cipher / rot-13
-- given a text -> key -> alphabet return the encrypted text
caesarEnc :: String -> Int -> String -> String
caesarEnc t k a = [ enc (sa k a) c | c <- t ]
  where
    -- given an alphabet a containing pairs of original and encoded chars 
    -- and a char c to encode return the encoded char
    enc :: [(Char, Char)] -> Char -> Char
    enc a c
      | c == ' ' = ' '
      | otherwise = let (_,w) = head (filter (\x -> let (v,_) = x in v==c) a)
                    in w

-- caesar cipher / rot-13
-- given a text -> key -> alphabet return the decrypted text
caesarDec :: String -> Int -> String -> String
caesarDec t k a = [ dec (sa k a) c | c <- t ]
  where
    -- given an alphabet a containing pairs of original and encoded chars
    -- and a char c to decode return the decoded char
    dec :: [(Char, Char)] -> Char -> Char
    dec a c
        | c == ' ' = ' '
        | otherwise = let (v,_) = head (filter (\x -> let (_,w) = x in w==c) a)
                      in v

-- vigenère cipher
-- given a text -> key -> alphabet return the encrypted text
vigenèreEnc :: String -> String -> String -> String
vigenèreEnc t k a = foldl (++) "" 
                    [ caesarEnc [c] (i kc a) a | (c,kc) <- zip t (kt t k) ]

-- vigenère cipher
-- given a text -> key -> alphabet return the decrypted text
vigenèreDec :: String -> String -> String -> String
vigenèreDec t k a = [ dec c kc a | (c,kc) <- zip t (kt t k) ]
  where
    dec :: Char -> Char -> String -> Char
    dec c kc a = let (u,_) = head (filter (\x -> let (u,v) = x in v==c)
                                          (sa (i kc a) a))
                 in u

-- given a text t and a key k
-- return a the key at the length of t
kt :: String -> String -> String
kt t k = (++) (foldl (++)
                     ""
                     (replicate (div (length t) (length k)) k))
              (let (r,_) = splitAt (mod (length t) (length k)) k in r)

-- given an alphabet shift it k times to the left
-- and return pairs containing the original and encoded char
sa :: Int -> String -> [(Char, Char)]
sa k a = let (x,y) = splitAt k a
        in zip a (y ++ x)

-- given a char c and a String s which contains c return the index of c
i :: Char -> String -> Int
i c s = let (_,n) = head (filter (\x -> let (a,_) = x in a ==c)
                                 (zip s [0..length s - 1]))
        in n


-- cryptanalysis
----------------

-- given a text t and an alphabet return the decrypted text
breakCaesar :: String -> String -> String
breakCaesar t a = head (filter (`cs` "die") (p t a))
  where
    -- given a text t and an alphabet a
    -- return a list of possible decryptions
    p :: String -> String -> [String]
    p t a = [ caesarDec t k a | k <- [1..length a - 1] ]

-- given a text t and a word w check if t contains w
cs :: String -> String -> Bool
cs t w = w `elem` ws t (length w)

-- given a text t -> length of word
-- compute the words with length l at the indices 0..upper bound
ws :: String -> Int -> [String]
ws t l = [extr t i l | i <- [0..length t - l] ]

-- given a text t -> index i -> word length l
-- return the word at the length l at the index i
extr :: String -> Int -> Int -> String
extr t i l = let (_,b) = splitAt i t
             in (let (a,_) = splitAt l b in a)
