module Diamond where

diamond c = mirror (map mirror (chain c))

mirror xs = xs ++ tail (reverse xs)

chain 'A' = ["A"]
chain 'B' = [" A","B "]
chain 'C' = [drop 0 (replicate 2 ' ' ++ ['A'] ++ replicate 0 ' '),
             drop 1 (replicate 2 ' ' ++ ['B'] ++ replicate 1 ' '),
             drop 2 (replicate 2 ' ' ++ ['C'] ++ replicate 2 ' ')]
