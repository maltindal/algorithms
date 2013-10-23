-- text as a list with length n
-- word/pattern with length m
-- where m << n

-- systematic search from the beginning of the text
-- return the position of the occurence of the text

-- possible problems with text, e.g. slow access or in a stream

-- given a word w and text t compute the indices where w occurs in t
naiveSearch :: String -> String -> [Int]
naiveSearch w t = map (\x -> let (i,_) = x in i) (matches w t)
  where
    matches :: String -> String -> [(Int, String)]
    matches w t = filter (\x -> let (_,y) = x in y == w)
                         (zip [0..length t - 1] (ws t (length w)))

-- given a text t -> length of word
-- compute the words with length l at the indices 0..upper bound
ws :: String -> Int -> [String]
ws t l = [extr t i l | i <- [0..length t - l] ]

-- given a text t -> index i -> word length l
-- return the word at the length l at the index i
extr :: String -> Int -> Int -> String
extr t i l = let (_,b) = splitAt i t
             in (let (a,_) = splitAt l b in a)
