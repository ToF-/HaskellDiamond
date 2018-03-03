module Diamond where
import Data.Char (ord)

diamond c = mirror (map mirror (chain c))

mirror xs = xs ++ tail (reverse xs)

chain c = zipWith pad [0..] ['A'..c]
    where l = ord c - ord 'A'
          pad n c = drop n (spaces l ++ [c] ++ spaces n) 
          spaces = flip replicate ' ' 
