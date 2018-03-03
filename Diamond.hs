module Diamond where
import Data.Char (ord)

diamond c = mirror (map mirror (chain c))

mirror xs = xs ++ tail (reverse xs)

chain 'A' = ["A"]
chain 'B' = [" A","B "]
chain 'C' = zipWith pad [0..] ['A'..'C']
    where l = ord 'C' - ord 'A'
          pad n c = drop n (replicate l ' ' ++ [c] ++ replicate n ' ') 
