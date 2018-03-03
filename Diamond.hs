module Diamond where
import Data.Char (ord)

diamond c = mirror (map mirror (chain c))

mirror xs = xs ++ tail (reverse xs)

chain 'A' = ["A"]
chain 'B' = [" A","B "]
chain 'C' = [drop 0 (replicate l ' ' ++ ['A'] ++ replicate 0 ' '),
             drop 1 (replicate l ' ' ++ ['B'] ++ replicate 1 ' '),
             drop 2 (replicate l ' ' ++ ['C'] ++ replicate 2 ' ')]
    where l = ord 'C' - ord 'A'
